open Virtual_dom.Vdom;

exception CallbackError;

let view =
  Node.span(
    ~attrs=[Attr.id("font-specimen"), Attr.class_("code")],
    [Node.text("X")],
  ) /* */;
