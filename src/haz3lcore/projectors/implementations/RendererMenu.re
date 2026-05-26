open Util;
open Virtual_dom.Vdom;

/* Menu DOM helpers shared between rich-probe renderers that use the
 * floating column-menu pattern (TableRenderer, ListRenderer). The
 * `column-menu` CSS class drives positioning + the click-outside
 * listener machinery — that name is kept so existing styles keep
 * applying. Per-renderer extras (e.g. `list-menu`, `column-menu-floating`)
 * are passed in as `extra_classes`. */

type menu_data('a) = list(Menu.item('a));

let menu_trigger_button =
    (~id: string, ~title: string, ~on_click: unit => Ui_effect.t(unit))
    : Node.t =>
  Node.div(
    ~attrs=[
      Attr.id(id),
      /* `menu-trigger` exempts the button from MenuListener's
       * click-outside detection so opening + closing both flow
       * through the same action. */
      Attr.classes(["icon", "closure-nav-button", "menu-trigger"]),
      Attr.on_click(_ => on_click()),
      Attr.title(title),
    ],
    [Node.text("⋮")],
  );

let dir_class = (dir: Menu.open_direction): string =>
  switch (dir) {
  | {vertical: `Down, horizontal: `Right} => "cm-down-right"
  | {vertical: `Down, horizontal: `Left} => "cm-down-left"
  | {vertical: `Up, horizontal: `Right} => "cm-up-right"
  | {vertical: `Up, horizontal: `Left} => "cm-up-left"
  };

let floating_menu_node =
    (
      ~menu_button_id: string,
      ~menu_state: Menu.t,
      ~items: menu_data(unit => Ui_effect.t(unit)),
      ~inject_menu_action: Menu.action => Ui_effect.t(unit),
      ~extra_classes: list(string)=[],
      ~extra_attrs: list(Attr.t)=[],
      (),
    )
    : Node.t => {
  let dir =
    Menu.direction_from_id(
      ~menu_height=200.0,
      ~menu_width=180.0,
      menu_button_id,
    );
  let menu_nodes =
    Menu.render(
      ~inject_action=thunk => thunk(),
      ~inject_menu=inject_menu_action,
      ~item_class="named-menu-item",
      ~items,
      menu_state,
    );
  Node.div(
    ~attrs=
      [
        Attr.classes(
          ["context-menu", "column-menu", dir_class(dir)] @ extra_classes,
        ),
      ]
      @ extra_attrs,
    [WebUtil.div_c("group", [WebUtil.div_c("contents", menu_nodes)])],
  );
};

/* Wires up the document-level click-outside + keyboard listeners.
 * When the menu is closed `items` is unused, so callers can pass an
 * empty list. */
let sync_listener =
    (
      ~menu_open: bool,
      ~on_close: Ui_effect.t(unit),
      ~items: menu_data(unit => Ui_effect.t(unit)),
      ~inject_menu_action: Menu.action => Ui_effect.t(unit),
      ~menu_state: Menu.t,
    )
    : unit => {
  let handle_key = (key: string): option(Ui_effect.t(unit)) =>
    Menu.key_dispatcher(
      ~items,
      ~dispatch_menu=inject_menu_action,
      ~dispatch_action=thunk => thunk(),
      menu_state,
      key,
    );
  ColumnMenuListener.sync(~menu_open, ~on_close, ~handle_key, ());
};
