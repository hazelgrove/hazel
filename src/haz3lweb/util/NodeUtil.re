open Virtual_dom.Vdom;

let svg = (attrs, children) =>
  Node.create_svg("svg", ~attr=Attr.many(attrs), children);

let stop = attrs => Node.create_svg("stop", ~attr=Attr.many(attrs), []);
