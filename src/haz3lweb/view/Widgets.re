open Virtual_dom.Vdom;
open Node;
open Util.Web;

let button = (~tooltip="", icon, action) =>
  div(
    ~attrs=[
      clss(["icon"]),
      Attr.on_mousedown(action),
      Attr.title(tooltip),
    ],
    [icon],
  );

let button_named = (~tooltip="", icon, action) =>
  div(
    ~attrs=[clss(["named-menu-item"]), Attr.on_click(action)],
    [button(icon, _ => Effect.Ignore), div([text(tooltip)])],
  );

let button_d = (~tooltip="", icon, action, ~disabled: bool) =>
  div(
    ~attrs=[
      clss(["icon"] @ (disabled ? ["disabled"] : [])),
      Attr.title(tooltip),
      Attr.on_mousedown(_ => unless(disabled, action)),
    ],
    [icon],
  );

let link = (~tooltip="", icon, url) =>
  div(
    ~attrs=[clss(["icon"])],
    [
      a(
        ~attrs=Attr.[href(url), title(tooltip), create("target", "_blank")],
        [icon],
      ),
    ],
  );

let toggle = (~tooltip="", label, active, action) =>
  div(
    ~attrs=[
      clss(["toggle-switch"] @ (active ? ["active"] : [])),
      Attr.on_click(action),
      Attr.title(tooltip),
    ],
    [div(~attrs=[clss(["toggle-knob"])], [text(label)])],
  );

let toggle_named = (~tooltip="", icon, active, action) =>
  div(
    ~attrs=[
      clss(["named-menu-item"] @ (active ? ["active"] : [])),
      Attr.on_click(action),
    ],
    [toggle(icon, active, _ => Effect.Ignore), div([text(tooltip)])],
  );

let file_select_button = (~tooltip="", id, icon, on_input) => {
  /* https://stackoverflow.com/questions/572768/styling-an-input-type-file-button */
  label(
    ~attrs=[Attr.for_(id)],
    [
      Vdom_input_widgets.File_select.single(
        ~extra_attrs=[Attr.class_("file-select-button"), Attr.id(id)],
        ~accept=[`Extension("json")],
        ~on_input,
        (),
      ),
      div(~attrs=[clss(["icon"]), Attr.title(tooltip)], [icon]),
    ],
  );
};

let file_select_button_named = (~tooltip="", id, icon, on_input) =>
  div(
    ~attrs=[clss(["named-menu-item"])],
    [file_select_button(id, icon, on_input), div([text(tooltip)])],
  );
