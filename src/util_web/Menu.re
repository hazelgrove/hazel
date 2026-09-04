open Util;
open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;
open Js_of_ocaml;
open Virtual_dom.Vdom;
open WebUtil;

/* Generic transient menu: state, items, rendering, keyboard handling.
 *
 * Shared by the editor context menu (right-click, Cmd+.) and the table
 * column menu (⋮). Both menus look and behave the same: a vertical list
 * of selectable rows with optional dividers, an optional right-aligned
 * decoration (shortcut chip or → submenu indicator), arrow-key
 * navigation, Enter to activate, Escape to close.
 *
 * Items are a tree: a `Submenu` carries its `children` inline. The Menu
 * owns the current path (submenu breadcrumb) and selected index. The
 * caller never destructures the state — they just store one `Menu.t`,
 * forward `Menu.action` updates through `Menu.update`, and pass the root
 * item tree to `Menu.render` / `Menu.handle_key`. A `← Back` row is
 * auto-prepended when the menu is in a non-root path. */

/* ============================================================
 * State
 * ============================================================ */

[@deriving (show({with_path: false}), sexp, yojson)]
type state = {
  selected_idx: int,
  path: list(string),
};

/* None = closed; Some({selected_idx, path}) = open. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = option(state);

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | Toggle
  | Open
  | Close
  | Up
  | Down
  | EnterSubmenu(string) /* push child label onto path */
  | BackSubmenu /* pop one level */
  | SetSelected(int);

let opened: t =
  Some({
    selected_idx: 0,
    path: [],
  });
let is_open = (m: t): bool => m != None;
let path = (m: t): list(string) =>
  switch (m) {
  | None => []
  | Some({path, _}) => path
  };
let selected = (m: t): int =>
  switch (m) {
  | None => 0
  | Some({selected_idx, _}) => selected_idx
  };

let parent_of_path = (path: list(string)): list(string) =>
  switch (List.rev(path)) {
  | []
  | [_] => []
  | [_, ...rest] => List.rev(rest)
  };

/* ============================================================
 * Items
 * ============================================================
 *
 * `'a` is the action payload of leaf rows; the caller decides how to
 * dispatch it. A `Back` row is never user-constructed — Menu synthesises
 * it when rendering a non-root path. */
type item('a) =
  | Action({
      label: string,
      decoration: option(string),
      tooltip: option(string),
      on_hover: bool,
      enabled: bool,
      action: 'a,
    })
  | Submenu({
      label: string,
      tooltip: option(string),
      children: list(item('a)),
    })
  | Divider;

let action_item =
    (~decoration=?, ~tooltip=?, ~on_hover=false, ~enabled=true, label, action) =>
  Action({
    label,
    decoration,
    tooltip,
    on_hover,
    enabled,
    action,
  });

let submenu_item = (~tooltip=?, label, children) =>
  Submenu({
    label,
    tooltip,
    children,
  });

let divider = Divider;

/* Walk the path through the item tree, returning the items visible at
 * that path. If the path doesn't resolve (stale label, etc.), falls back
 * to the root list. */
let rec items_at =
        (path: list(string), items: list(item('a))): list(item('a)) =>
  switch (path) {
  | [] => items
  | [name, ...rest] =>
    let child =
      List.find_opt(
        fun
        | Submenu({label, _}) => label == name
        | _ => false,
        items,
      );
    switch (child) {
    | Some(Submenu({children, _})) => items_at(rest, children)
    | _ => items
    };
  };

/* Visible item shape used internally for rendering + indexing: includes
 * a `Back` marker when path != []. Not exposed publicly so callers can't
 * confuse themselves about who owns Back. */
type visible_item('a) =
  | VBack
  | VAction({
      label: string,
      decoration: option(string),
      tooltip: option(string),
      on_hover: bool,
      enabled: bool,
      action: 'a,
    })
  | VSubmenu({
      label: string,
      tooltip: option(string),
      submenu_name: string,
    })
  | VDivider;

let to_visible = (it: item('a)): visible_item('a) =>
  switch (it) {
  | Action({label, decoration, tooltip, on_hover, enabled, action}) =>
    VAction({
      label,
      decoration,
      tooltip,
      on_hover,
      enabled,
      action,
    })
  | Submenu({label, tooltip, _}) =>
    VSubmenu({
      label,
      tooltip,
      submenu_name: label,
    })
  | Divider => VDivider
  };

/* The list of rows to render at the current path, with Back synthesised
 * when nested. */
let visible_items =
    (~items: list(item('a)), model: t): list(visible_item('a)) => {
  let p = path(model);
  let here = items_at(p, items) |> List.map(to_visible);
  p == [] ? here : [VBack, ...here];
};

let is_visible_selectable = (v: visible_item('a)): bool =>
  switch (v) {
  | VBack => true
  | VAction({enabled, _}) => enabled
  | VSubmenu(_) => true
  | VDivider => false
  };

let count_selectable_visible = (vs: list(visible_item('a))): int =>
  List.fold_left((n, v) => is_visible_selectable(v) ? n + 1 : n, 0, vs);

let nth_selectable_visible =
    (vs: list(visible_item('a)), idx: int): option(visible_item('a)) => {
  let rec go = (vs, i) =>
    switch (vs) {
    | [] => None
    | [v, ...rest] when is_visible_selectable(v) =>
      i == 0 ? Some(v) : go(rest, i - 1)
    | [_, ...rest] => go(rest, i)
    };
  go(vs, idx);
};

let clamp_visible = (vs: list(visible_item('a)), idx: int): int => {
  let n = count_selectable_visible(vs);
  n == 0 ? 0 : max(0, min(idx, n - 1));
};

/* ============================================================
 * State updates
 * ============================================================
 *
 * `update` is pure — it doesn't clamp `selected_idx` against the visible
 * item count, because the items aren't always available at update time
 * (e.g. when called from a projector reducer that doesn't see the menu's
 * context). Render and keyboard handling clamp at use time, so callers
 * never need to. */
let update = (action: action, model: t): t => {
  let cur_path = path(model);
  switch (action) {
  | Toggle =>
    switch (model) {
    | None => opened
    | Some(_) => None
    }
  | Open => opened
  | Close => None
  | Up =>
    switch (model) {
    | None => None
    | Some({selected_idx, path}) =>
      Some({
        selected_idx: max(0, selected_idx - 1),
        path,
      })
    }
  | Down =>
    switch (model) {
    | None => None
    | Some({selected_idx, path}) =>
      Some({
        selected_idx: selected_idx + 1,
        path,
      })
    }
  | EnterSubmenu(label) =>
    Some({
      selected_idx: 0,
      path: cur_path @ [label],
    })
  | BackSubmenu =>
    Some({
      selected_idx: 0,
      path: parent_of_path(cur_path),
    })
  | SetSelected(i) =>
    switch (model) {
    | None => None
    | Some({path, _}) =>
      Some({
        selected_idx: max(0, i),
        path,
      })
    }
  };
};

/* ============================================================
 * Rendering
 * ============================================================ */

let shortcut_view = (text_: string) =>
  Node.span(~attrs=[clss(["menu-shortcut"])], [Node.text(text_)]);

let divider_view = () => Node.div(~attrs=[clss(["menu-divider"])], []);

/* `on_fire` is invoked at event-firing time, never at render time —
 * critical for callers whose actions carry side-effecting thunks. */
let pointerdown_attr = (on_fire: unit => Ui_effect.t(unit)) =>
  Attr.on_pointerdown(_ =>
    Effect.Many([Effect.Stop_propagation, Effect.Prevent_default, on_fire()])
  );

let item_classes = (~item_class: string, ~is_selected: bool, ~enabled: bool) =>
  [item_class]
  @ (is_selected ? ["selected"] : [])
  @ (enabled ? [] : ["disabled"]);

let row_view =
    (
      ~item_class: string,
      ~is_selected: bool,
      ~enabled: bool,
      ~tooltip: option(string),
      ~decoration: option(string),
      ~on_pointerdown: unit => Ui_effect.t(unit),
      ~on_hover: option(Ui_effect.t(unit)),
      label: string,
    ) => {
  let title_attrs =
    switch (tooltip) {
    | Some(t) => [Attr.title(t)]
    | None => []
    };
  let hover_attrs =
    switch (on_hover) {
    | Some(eff) => [Attr.on_mouseenter(_ => eff)]
    | None => []
    };
  let children =
    [Node.text(label)]
    @ (
      switch (decoration) {
      | Some(s) => [shortcut_view(s)]
      | None => []
      }
    );
  Node.div(
    ~attrs=
      [
        clss(item_classes(~item_class, ~is_selected, ~enabled)),
        pointerdown_attr(on_pointerdown),
      ]
      @ title_attrs
      @ hover_attrs,
    children,
  );
};

/* Render rows visible at the model's path. The caller passes the entire
 * item tree; Menu walks the path, synthesises Back when nested, and
 * indexes selected_idx across only the selectable rows. */
let render =
    (
      ~inject_action: 'a => Ui_effect.t(unit),
      ~inject_menu: action => Ui_effect.t(unit),
      ~item_class: string,
      ~items: list(item('a)),
      model: t,
    )
    : list(Node.t) => {
  let vs = visible_items(~items, model);
  let selected_idx = clamp_visible(vs, selected(model));
  let (_, rendered) =
    List.fold_left_map(
      (sel_idx, v) =>
        switch (v) {
        | VDivider => (sel_idx, divider_view())
        | VBack => (
            sel_idx + 1,
            row_view(
              ~item_class,
              ~is_selected=sel_idx == selected_idx,
              ~enabled=true,
              ~tooltip=None,
              ~decoration=None,
              ~on_pointerdown=() => inject_menu(BackSubmenu),
              ~on_hover=Some(inject_menu(SetSelected(sel_idx))),
              "← Back",
            ),
          )
        | VAction({label, decoration, tooltip, on_hover, enabled, action}) => (
            sel_idx + 1,
            row_view(
              ~item_class,
              ~is_selected=enabled && sel_idx == selected_idx,
              ~enabled,
              ~tooltip,
              ~decoration,
              ~on_pointerdown=
                () => enabled ? inject_action(action) : Effect.Ignore,
              ~on_hover=
                on_hover ? Some(inject_menu(SetSelected(sel_idx))) : None,
              label,
            ),
          )
        | VSubmenu({label, tooltip, submenu_name}) => (
            sel_idx + 1,
            row_view(
              ~item_class,
              ~is_selected=sel_idx == selected_idx,
              ~enabled=true,
              ~tooltip,
              ~decoration=Some("→"),
              ~on_pointerdown=() => inject_menu(EnterSubmenu(submenu_name)),
              ~on_hover=Some(inject_menu(SetSelected(sel_idx))),
              label,
            ),
          )
        },
      0,
      vs,
    );
  rendered;
};

/* ============================================================
 * Keyboard handling
 * ============================================================ */

type key_result('a) =
  | MenuUpdate(action)
  | RunAction('a)
  | Unhandled;

let handle_key =
    (~items: list(item('a)), key: Key.key, model: t): key_result('a) =>
  switch (model) {
  | None => Unhandled
  | Some({selected_idx, path}) =>
    let vs = visible_items(~items, model);
    let n = count_selectable_visible(vs);
    /* Use the clamped index for both selection lookup AND the next
     * step, so a stale unclamped stored value can't stall ArrowUp/Down. */
    let idx = clamp_visible(vs, selected_idx);
    let selected_item = nth_selectable_visible(vs, idx);
    switch (key) {
    | Key.D("Escape") => MenuUpdate(Close)
    | Key.D("ArrowUp") =>
      n == 0 ? Unhandled : MenuUpdate(SetSelected(max(0, idx - 1)))
    | Key.D("ArrowDown") =>
      n == 0 ? Unhandled : MenuUpdate(SetSelected(min(n - 1, idx + 1)))
    | Key.D("ArrowRight") =>
      switch (selected_item) {
      | Some(VSubmenu({submenu_name, _})) =>
        MenuUpdate(EnterSubmenu(submenu_name))
      | _ => Unhandled
      }
    | Key.D("ArrowLeft") => path == [] ? Unhandled : MenuUpdate(BackSubmenu)
    | Key.D("Enter") =>
      switch (selected_item) {
      | Some(VAction({enabled: true, action, _})) => RunAction(action)
      | Some(VSubmenu({submenu_name, _})) =>
        MenuUpdate(EnterSubmenu(submenu_name))
      | Some(VBack) => MenuUpdate(BackSubmenu)
      | Some(VAction({enabled: false, _}))
      | Some(VDivider)
      | None => MenuUpdate(Close)
      }
    | _ => Unhandled
    };
  };

/* Adapter for MenuListener.sync(~handle_key). Returns Some(effect) for
 * handled keys, None to let other listeners see them. */
let key_dispatcher =
    (
      ~items: list(item('a)),
      ~dispatch_menu: action => Ui_effect.t(unit),
      ~dispatch_action: 'a => Ui_effect.t(unit),
      model: t,
      key_str: string,
    )
    : option(Ui_effect.t(unit)) =>
  switch (handle_key(~items, Key.D(key_str), model)) {
  | MenuUpdate(action) => Some(dispatch_menu(action))
  | RunAction(a) => Some(dispatch_action(a))
  | Unhandled => None
  };

/* ============================================================
 * Viewport-aware open direction
 * ============================================================ */

type vertical_dir = [
  | `Up
  | `Down
];
type horizontal_dir = [
  | `Left
  | `Right
];
type open_direction = {
  vertical: vertical_dir,
  horizontal: horizontal_dir,
};

type available_space = {
  above: float,
  below: float,
  left: float,
  right: float,
};

let main_viewport_rect = () =>
  switch (JsUtil.get_elem_by_id_opt("main")) {
  | Some(main) => main##getBoundingClientRect
  | None =>
    Js.Unsafe.obj([|
      ("top", Js.Unsafe.inject(0.0)),
      ("bottom", Js.Unsafe.inject(Js.Unsafe.global##.innerHeight)),
      ("left", Js.Unsafe.inject(0.0)),
      ("right", Js.Unsafe.inject(Js.Unsafe.global##.innerWidth)),
    |])
  };

let space_from =
    (
      ~anchor_top: float,
      ~anchor_bot: float,
      ~anchor_left: float,
      ~anchor_right: float,
    )
    : available_space => {
  let main = main_viewport_rect();
  {
    above: anchor_top -. main##.top,
    below: main##.bottom -. anchor_bot,
    left: anchor_right -. main##.left,
    right: main##.right -. anchor_left,
  };
};

let direction_of =
    (~menu_height: float, ~menu_width: float, space: available_space)
    : open_direction => {
  vertical: space.below >= menu_height ? `Down : `Up,
  horizontal: space.right >= menu_width ? `Right : `Left,
};

let direction_from_elem =
    (~menu_height: float, ~menu_width: float, elem: Js.t(Dom_html.element))
    : open_direction => {
  let rect = elem##getBoundingClientRect;
  let space =
    space_from(
      ~anchor_top=rect##.top,
      ~anchor_bot=rect##.bottom,
      ~anchor_left=rect##.left,
      ~anchor_right=rect##.right,
    );
  direction_of(~menu_height, ~menu_width, space);
};

let direction_from_id =
    (~menu_height: float, ~menu_width: float, id: string): open_direction =>
  switch (JsUtil.get_elem_by_id_opt(id)) {
  | Some(elem) => direction_from_elem(~menu_height, ~menu_width, elem)
  | None => {
      vertical: `Down,
      horizontal: `Right,
    }
  };
