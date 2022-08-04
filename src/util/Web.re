open Virtual_dom.Vdom;
open Node;

let clss = Attr.classes;

let div_c = cls => div([Attr.class_(cls)]);
let span_c = cls => span([Attr.class_(cls)]);

let div_if = (p, ats, ns) => p ? div(ats, ns) : div([], []);
let span_if = (p, ats, ns) => p ? span(ats, ns) : span([], []);
