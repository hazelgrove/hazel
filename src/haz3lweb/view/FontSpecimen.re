open Virtual_dom.Vdom;

let view = id => Node.span(~attrs=[Attr.id(id)], [Node.text("X")]);
