open Virtual_dom.Vdom;

let code = (code: string) => {
  Node.span(~attr=Attr.class_("exercise-code"), [Node.text(code)]);
};
