open Virtual_dom.Vdom;
open Node;

type cls = string;

let cell_id: string;
let card_dom_id: string;
let font_specimen_id: string;
let history_body_id: string;
let caret_id: string;
let ci_id: string;
let cur_selected_id: string;
let code_root_id: string;

let div_if: (bool, list(Attr.t), list(t)) => t;
