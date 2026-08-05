open Haz3lcore;
open Example;
open ExplainThisForm;

let pat_con = pat("p_con");
let pat_arg = pat("p_arg");
let conapp_pat_coloring_ids =
    (~x_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(pat_con), x_id),
  (Piece.id(pat_arg), arg_id),
];
let conapp_pat_form = [pat_con, mk_ap_pat([[pat_arg]])];
let conapp_pat = (~x_id: Id.t, ~arg_id: Id.t): form => {
  id: ApConsPat,
  syntactic_form: conapp_pat_form,
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "Only expressions that match the [*constructor*](%s) with an *argument* matching the [*argument pattern*](%s) match this *constructor application pattern*.",
      Id.to_string(x_id),
      Id.to_string(arg_id),
    ),
  examples: [],
};

let pat_fun = pat("fun");
let pat_arg = pat("p_arg");
let funapp_pat_coloring_ids =
    (~x_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(pat_fun), x_id),
  (Piece.id(pat_arg), arg_id),
];
let funapp_pat_form = [pat_fun, mk_ap_pat([[pat_arg]])];
let funapp_pat = (~x_id: Id.t, ~arg_id: Id.t): form => {
  id: ApConsPat,
  syntactic_form: funapp_pat_form,
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "Defines a function [*function*](%s) with [*arguments*](%s).",
      Id.to_string(x_id),
      Id.to_string(arg_id),
    ),
  examples: [],
};

let conaps = (~x_id: Id.t, ~arg_id: Id.t): group => {
  id: ApConsPat,
  forms: [conapp_pat(~x_id, ~arg_id)],
};

let funaps = (~x_id: Id.t, ~arg_id: Id.t): group => {
  id: ApFuncPat,
  forms: [funapp_pat(~x_id, ~arg_id)],
};
