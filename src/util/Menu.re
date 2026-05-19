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
 * Submenu navigation is path-driven: items of kind `Submenu({target_path})`
 * push a path onto the model; a `Back({to_path})` row pops back to a
 * shallower path. Callers compute the visible items from the current path
 * — the framework just tracks the path and the selected index. */

[@deriving (show({with_path: false}), sexp, yojson)]
type state = {
  selected_idx: int,
  path: list(string),
};

/* Menu state: None = closed; Some({selected_idx, path}) = open at `path`
 * with row `selected_idx` highlighted. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = option(state);

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | Toggle
  | Open
  | Close
  | Up
  | Down
  | EnterSubmenu(list(string))
  | BackSubmenu(list(string))
  | SetSelected(int);

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

let init = (path: list(string)): state => {
  selected_idx: 0,
  path,
};

/* Pure state update. Selection clamping against the actual item count is
 * the caller's responsibility — `clamp_against` below is the helper. */
let update = (action: action, model: t): t =>
  switch (action) {
  | Toggle =>
    switch (model) {
    | None => Some(init([]))
    | Some(_) => None
    }
  | Open => Some(init([]))
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
  | EnterSubmenu(path) => Some(init(path))
  | BackSubmenu(path) => Some(init(path))
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

/* Item types. `'a` is the action carried by selectable rows; the caller
 * decides how to dispatch it (e.g. an editor `Action.t`, or a thunk
 * `unit => Effect.t(unit)`). */
type item('a) =
  | Action({
      label: string,
      /* Right-aligned decoration text (keyboard shortcut, etc.) */
      decoration: option(string),
      /* Tooltip rendered as Attr.title */
      tooltip: option(string),
      /* If true, mouse hover updates the selected index */
      on_hover: bool,
      /* If false, the row is shown dimmed and pointerdown is ignored */
      enabled: bool,
      action: 'a,
    })
  | Submenu({
      label: string,
      tooltip: option(string),
      target_path: list(string),
    })
  | Back({to_path: list(string)})
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

let submenu_item = (~tooltip=?, label, target_path) =>
  Submenu({
    label,
    tooltip,
    target_path,
  });

let back_item = (~to_path=[], ()) => Back({to_path: to_path});

let divider = Divider;

let is_selectable = (item: item('a)): bool =>
  switch (item) {
  | Action({enabled, _}) => enabled
  | Submenu(_)
  | Back(_) => true
  | Divider => false
  };

let count_selectable = (items: list(item('a))): int =>
  List.fold_left((n, it) => is_selectable(it) ? n + 1 : n, 0, items);

let clamp_against = (items: list(item('a)), idx: int): int => {
  let n = count_selectable(items);
  n == 0 ? 0 : max(0, min(idx, n - 1));
};

/* Find the selectable item at the given (0-indexed) selectable position. */
let nth_selectable = (items: list(item('a)), idx: int): option(item('a)) => {
  let rec go = (items, i) =>
    switch (items) {
    | [] => None
    | [it, ...rest] when is_selectable(it) =>
      i == 0 ? Some(it) : go(rest, i - 1)
    | [_, ...rest] => go(rest, i)
    };
  go(items, idx);
};

/* ============================================================
 * Rendering
 * ============================================================ */

let shortcut_view = (text_: string) =>
  Node.span(~attrs=[clss(["menu-shortcut"])], [Node.text(text_)]);

let divider_view = () => Node.div(~attrs=[clss(["menu-divider"])], []);

/* Wrap a click/pointerdown handler so the document-level click-outside
 * listener doesn't fire, and so the surrounding editor doesn't steal
 * the click. `on_fire` is invoked at event-firing time — callers must
 * not evaluate the dispatched effect eagerly (e.g. by composing with
 * `inject_action(action)` outside this closure), otherwise side effects
 * like prompts will run at render time. */
let pointerdown_attr = (on_fire: unit => Ui_effect.t(unit)) =>
  Attr.on_pointerdown(_ =>
    Effect.Many([Effect.Stop_propagation, Effect.Prevent_default, on_fire()])
  );

let item_classes = (~item_class: string, ~is_selected: bool, ~enabled: bool) =>
  [item_class]
  @ (is_selected ? ["selected"] : [])
  @ (enabled ? [] : ["disabled"]);

/* Render a row that looks like an action item (also used for submenu /
 * back rows so they style identically). */
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

/* Render the menu's items into a list of Vdom nodes. Indices used for
 * selection skip dividers so `selected_idx` indexes selectable rows only. */
let render =
    (
      ~inject_action: 'a => Ui_effect.t(unit),
      ~inject_menu: action => Ui_effect.t(unit),
      ~item_class: string,
      ~selected_idx: int,
      items: list(item('a)),
    )
    : list(Node.t) => {
  let (_, rendered) =
    List.fold_left_map(
      (sel_idx, it) =>
        switch (it) {
        | Divider => (sel_idx, divider_view())
        | Action({label, decoration, tooltip, on_hover, enabled, action}) => (
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
        | Submenu({label, tooltip, target_path}) => (
            sel_idx + 1,
            row_view(
              ~item_class,
              ~is_selected=sel_idx == selected_idx,
              ~enabled=true,
              ~tooltip,
              ~decoration=Some("→"),
              ~on_pointerdown=() => inject_menu(EnterSubmenu(target_path)),
              ~on_hover=Some(inject_menu(SetSelected(sel_idx))),
              label,
            ),
          )
        | Back({to_path}) => (
            sel_idx + 1,
            row_view(
              ~item_class,
              ~is_selected=sel_idx == selected_idx,
              ~enabled=true,
              ~tooltip=None,
              ~decoration=None,
              ~on_pointerdown=() => inject_menu(BackSubmenu(to_path)),
              ~on_hover=Some(inject_menu(SetSelected(sel_idx))),
              "← Back",
            ),
          )
        },
      0,
      items,
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

/* Translate a Key.key into either a menu-state update or an action to
 * run. Caller wires this into a key dispatcher (e.g. MenuListener's
 * `handle_key`) and maps `RunAction` / `MenuUpdate` to its own dispatch. */
let handle_key =
    (~items: list(item('a)), key: Key.key, model: t): key_result('a) =>
  switch (model) {
  | None => Unhandled
  | Some({selected_idx, _}) =>
    switch (key) {
    | Key.D("Escape") => MenuUpdate(Close)
    | Key.D("ArrowUp") => MenuUpdate(Up)
    | Key.D("ArrowDown") => MenuUpdate(Down)
    | Key.D("Enter") =>
      let idx = clamp_against(items, selected_idx);
      switch (nth_selectable(items, idx)) {
      | Some(Action({enabled: true, action, _})) => RunAction(action)
      | Some(Submenu({target_path, _})) =>
        MenuUpdate(EnterSubmenu(target_path))
      | Some(Back({to_path})) => MenuUpdate(BackSubmenu(to_path))
      | Some(Action({enabled: false, _}))
      | Some(Divider)
      | None => MenuUpdate(Close)
      };
    | _ => Unhandled
    }
  };

/* ============================================================
 * Viewport-aware open direction
 * ============================================================
 *
 * Shared by menus that need to flip when they'd otherwise overflow the
 * #main viewport. Editor (caret-anchored) and column (header-anchored)
 * menus pick their own anchor point but route through the same logic. */

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

/* Available space from a (viewport-coordinate) anchor point. `anchor_top`
 * is the top edge where a downward-opening menu would start; `anchor_bot`
 * is the bottom edge where an upward-opening menu would start. They can
 * differ (caret height) or be equal (element top edge). */
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

/* Pick a direction given the menu's footprint estimate. */
let direction_of =
    (~menu_height: float, ~menu_width: float, space: available_space)
    : open_direction => {
  vertical: space.below >= menu_height ? `Down : `Up,
  horizontal: space.right >= menu_width ? `Right : `Left,
};

/* Direction from an HTML element's bounding rect — opens downward from
 * the element's bottom, upward from its top. Used by element-anchored
 * menus like the table column menu. */
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

/* Direction from an element looked up by id, falling back to down-right
 * if the element isn't in the DOM yet. */
let direction_from_id =
    (~menu_height: float, ~menu_width: float, id: string): open_direction =>
  switch (JsUtil.get_elem_by_id_opt(id)) {
  | Some(elem) => direction_from_elem(~menu_height, ~menu_width, elem)
  | None => {
      vertical: `Down,
      horizontal: `Right,
    }
  };

/* Adapt `handle_key` for MenuListener.sync's `handle_key` callback, which
 * speaks raw key strings and returns `option(Effect.t(unit))`. Caller
 * supplies how to dispatch a `MenuUpdate` and how to dispatch a
 * `RunAction`. */
let key_dispatcher =
    (
      ~items_at: list(string) => list(item('a)),
      ~dispatch_menu: action => Ui_effect.t(unit),
      ~dispatch_action: 'a => Ui_effect.t(unit),
      model: t,
      key_str: string,
    )
    : option(Ui_effect.t(unit)) =>
  switch (model) {
  | None => None
  | Some({path, _}) =>
    let items = items_at(path);
    switch (handle_key(~items, Key.D(key_str), model)) {
    | MenuUpdate(action) => Some(dispatch_menu(action))
    | RunAction(a) => Some(dispatch_action(a))
    | Unhandled => None
    };
  };
