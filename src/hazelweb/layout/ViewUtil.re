open Virtual_dom.Vdom;
open Node;

type cls = string;

let cell_id = "cell";
let card_dom_id = "card";
let font_specimen_id = "font-specimen";
let history_body_id = "history-body";
let caret_id = "caret";
let ci_id = "cursor-inspector";
let cur_selected_id = "cur-selected-entry";

let div_if = (p, ats, ns) => p ? div(ats, ns) : div([], []);
