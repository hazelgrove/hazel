open Virtual_dom.Vdom;
open Node;

type cls = string;

let cell_id: string;

let card_dom_id: string;

let div_if: (bool, list(Attr.t), list(t)) => t;
