open Virtual_dom.Vdom;
open Node;

let clss = Attr.classes;

let div_c = cls => div(~attrs=[Attr.class_(cls)]);
let span_c = cls => span(~attrs=[Attr.class_(cls)]);

let div_empty = div(~attrs=[Attr.create("style", "display:none")], []);

let div_if = (p, ats, ns) => p ? div(~attrs=[ats], ns) : div_empty;
let span_if = (p, ats, ns) => p ? span(~attrs=[ats], ns) : span([]);

let unless = (p, a) => p ? Effect.Many([]) : a;
