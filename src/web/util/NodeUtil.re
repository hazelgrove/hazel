open Virtual_dom.Vdom;

let svg = (attrs, children) => Node.create_svg("svg", attrs, children);

let stop = attrs => Node.create_svg("stop", attrs, []);
