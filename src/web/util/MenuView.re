open Util;
open Js_of_ocaml;
open Virtual_dom.Vdom;
open WebUtil;

/* Vdom rendering and DOM-position helpers for [Util.Menu]. The pure
 * menu state machine (types, update, handle_key) lives in util; this
 * module owns everything that touches Virtual_dom or the DOM. */

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
      ~inject_menu: Menu.action => Ui_effect.t(unit),
      ~item_class: string,
      ~items: list(Menu.item('a)),
      model: Menu.t,
    )
    : list(Node.t) => {
  let vs = Menu.visible_items(~items, model);
  let selected_idx = Menu.clamp_visible(vs, Menu.selected(model));
  let (_, rendered) =
    List.fold_left_map(
      (sel_idx, v: Menu.visible_item('a)) =>
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

/* Adapter for MenuListener.sync(~handle_key). Returns Some(effect) for
 * handled keys, None to let other listeners see them. */
let key_dispatcher =
    (
      ~items: list(Menu.item('a)),
      ~dispatch_menu: Menu.action => Ui_effect.t(unit),
      ~dispatch_action: 'a => Ui_effect.t(unit),
      model: Menu.t,
      key_str: string,
    )
    : option(Ui_effect.t(unit)) =>
  switch (Menu.handle_key(~items, Key.D(key_str), model)) {
  | MenuUpdate(action) => Some(dispatch_menu(action))
  | RunAction(a) => Some(dispatch_action(a))
  | Unhandled => None
  };

/* ============================================================
 * Viewport-aware open direction
 * ============================================================ */

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
    : Menu.available_space => {
  let main = main_viewport_rect();
  {
    above: anchor_top -. main##.top,
    below: main##.bottom -. anchor_bot,
    left: anchor_right -. main##.left,
    right: main##.right -. anchor_left,
  };
};

let direction_from_elem =
    (~menu_height: float, ~menu_width: float, elem: Js.t(Dom_html.element))
    : Menu.open_direction => {
  let rect = elem##getBoundingClientRect;
  let space =
    space_from(
      ~anchor_top=rect##.top,
      ~anchor_bot=rect##.bottom,
      ~anchor_left=rect##.left,
      ~anchor_right=rect##.right,
    );
  Menu.direction_of(~menu_height, ~menu_width, space);
};

let direction_from_id =
    (~menu_height: float, ~menu_width: float, id: string): Menu.open_direction =>
  switch (JsUtil.get_elem_by_id_opt(id)) {
  | Some(elem) => direction_from_elem(~menu_height, ~menu_width, elem)
  | None => {
      vertical: `Down,
      horizontal: `Right,
    }
  };
