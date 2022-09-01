open Virtual_dom.Vdom;
open Node;

let clss = Attr.classes;

let div_c = cls => div(~attr=Attr.class_(cls));
let span_c = cls => span(~attr=Attr.class_(cls));

let div_if = (p, ats, ns) => p ? div(~attr=Attr.many(ats), ns) : div([]);
let span_if = (p, ats, ns) =>
  p ? span(~attr=Attr.many(ats), ns) : span([]);

let unless = (p, a) => p ? Effect.Many([]) : a;
