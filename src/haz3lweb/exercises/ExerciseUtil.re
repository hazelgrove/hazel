open Virtual_dom.Vdom;

let code = (code: string) => {
  Node.span(~attrs=[Attr.class_("code")], [Node.text(code)]);
};

let equiv =
  Node.span(~attrs=[Attr.class_("equiv")], [Node.text(" ≡ ")]);
