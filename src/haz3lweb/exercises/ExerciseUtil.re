open Virtual_dom.Vdom;

let code = (code: string) => {
  Node.span(~attr=Attr.class_("exercise-code"), [Node.text(code)]);
};

let equiv = Node.span(~attr=Attr.class_("equiv"), [Node.text(" ≡ ")]);
