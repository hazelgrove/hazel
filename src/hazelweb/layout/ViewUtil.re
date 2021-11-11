open Virtual_dom.Vdom;
open Node;

type cls = string;

let cell_id = "cell";

let card_dom_id = "card";

let div_if = (p, ats, ns) => p ? div(ats, ns) : div([], []);
