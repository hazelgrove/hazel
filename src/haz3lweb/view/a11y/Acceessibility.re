open Virtual_dom.Vdom;
//open Node;
open Util.Web;
// open Util;
// open Haz3lcore;

let button_view = (~inject, query, name): Node.t => {
  Node.button(
    ~key=name,
    ~attr=
      Attr.many([
        Attr.classes(["button"]),
        Attr.on_mousedown(inject(Update.PerformQuery(query))),
      ]),
    [],
  );
};

let buttons_view = (~inject): Node.t =>
  Node.div(
    ~attr=clss(["buttons"]),
    [
      button_view(~inject, Query.CursorPos, "cursor position"),
      button_view(~inject, Query.Expression, "expression"),
    ],
  );
