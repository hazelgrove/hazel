open Virtual_dom.Vdom;

let view = id =>
  Node.span(~attrs=[Attr.id(id), Attr.class_("code")], [Node.text("X")]);
