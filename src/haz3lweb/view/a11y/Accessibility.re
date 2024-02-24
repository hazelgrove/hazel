open Virtual_dom.Vdom;
//open Node;
open Util.Web;
// open Util;
// open Haz3lcore;

let query_input_id = "query-input";

let button_view = (~inject, name): Node.t => {
  Node.button(
    ~attr=
      Attr.many([
        Attr.classes(["button"]),
        Attr.on_mousedown(_ => inject(UpdateAction.QueryInput)),
      ]),
    [Node.text(name)],
  );
};

let input_view = (id): Node.t => {
  Node.input(~attr=Attr.many([Attr.type_("text"), Attr.id(id)]), []);
};

let buttons_view = (~inject): Node.t => {
  Node.div(
    ~attr=clss(["query"]),
    [input_view(query_input_id), button_view(~inject, "Submit")],
  );
};

let view = (~inject): Node.t => buttons_view(~inject);
