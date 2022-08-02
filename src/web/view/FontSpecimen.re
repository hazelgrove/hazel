open Virtual_dom.Vdom;

let view = id => Node.span([Attr.id(id)], [Node.text("X")]);
