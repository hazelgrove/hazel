open Virtual_dom.Vdom;
//open Node;
open Util.Web;
// open Util;
// open Haz3lcore;

let button_view = (~inject, query, name): Node.t => {
  Node.button(
    ~attr=
      Attr.many([
        Attr.classes(["button"]),
        Attr.on_mousedown(_ => inject(Update.PerformQuery(query))),
      ]),
    [Node.text(name)],
  );
};

let buttons_view = (~inject): Node.t => {
  Node.div(
    ~attr=clss(["buttons"]),
    [
      button_view(~inject, Query.CursorPos, "cursor position"),
      button_view(~inject, Query.CursorInfo, "cursor info"),
      button_view(~inject, Query.CursorMove, "cursor move"),
      button_view(~inject, Query.ContextInfo, "context info"),
    ],
  );
};

let view = (~inject): Node.t => buttons_view(~inject);
