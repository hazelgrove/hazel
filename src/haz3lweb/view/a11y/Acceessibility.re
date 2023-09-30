open Virtual_dom.Vdom;
open Node;
open Util.Web;
// open Util;
// open Haz3lcore;

let button_view = (~inject, ~settings, name): Node.t => {
  Node.button(
    ~key=name,
    ~attr=
      Attr.many([
        Attr.classes(["button"]),
        Attr.on_mousedown(
          inject(Update.PerformAction(Jump(BindingSiteOfIndicatedVar))),
        ),
      ]),
    [],
  );
};

let buttons_view = (~inject, ~settings): Node.t =>
  Node.div(
    ~attr=clss(["buttons"]),
    [button_view(~inject, ~settings, "inspector")],
  );
