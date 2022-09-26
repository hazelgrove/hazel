open Virtual_dom.Vdom;
open Node;
open Util.Web;

let button = (~tooltip="", icon, action) =>
  div(
    ~attr=
      Attr.many([
        clss(["icon"]),
        Attr.on_mousedown(action),
        Attr.title(tooltip),
      ]),
    [icon],
  );

let button_d = (~tooltip="", icon, action, ~disabled: bool) =>
  div(
    ~attr=
      Attr.many([
        clss(["icon"] @ (disabled ? ["disabled"] : [])),
        Attr.title(tooltip),
        Attr.on_mousedown(_ => unless(disabled, action)),
      ]),
    [icon],
  );

let link = (~tooltip="", icon, url) =>
  div(
    ~attr=clss(["icon"]),
    [
      a(
        ~attr=
          Attr.many(
            Attr.[href(url), title(tooltip), create("target", "_blank")],
          ),
        [icon],
      ),
    ],
  );

let toggle = (~tooltip="", label, active, action) =>
  div(
    ~attr=
      Attr.many([
        clss(["toggle-switch"] @ (active ? ["active"] : [])),
        Attr.on_click(action),
        Attr.title(tooltip),
      ]),
    [div(~attr=clss(["toggle-knob"]), [text(label)])],
  );

let file_select_button = (~tooltip="", id, icon, on_input) => {
  /* https://stackoverflow.com/questions/572768/styling-an-input-type-file-button */
  label(
    ~attr=Attr.for_(id),
    [
      Vdom_input_widgets.File_select.single(
        ~extra_attrs=[Attr.class_("file-select-button"), Attr.id(id)],
        ~accept=[`Extension("json")],
        ~on_input,
        (),
      ),
      div(~attr=Attr.many([clss(["icon"]), Attr.title(tooltip)]), [icon]),
    ],
  );
};
