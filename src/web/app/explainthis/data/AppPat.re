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
let conapp_pat: form = {
  let explanation = "Only expressions that match the [*constructor*](%s) with an *argument* matching the [*argument pattern*](%s) match this *constructor application pattern*.";
  {
    id: ApConsPat,
    syntactic_form: [pat_con, mk_ap_pat([[pat_arg]])],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let pat_fun = pat("fun");
let pat_arg = pat("p_arg");
let funapp_pat_coloring_ids =
    (~x_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(pat_fun), x_id),
  (Piece.id(pat_arg), arg_id),
];
let funapp_pat: form = {
  let explanation = "Defines a function [*function*](%s) with [*arguments*](%s).";
  {
    id: ApConsPat,
    syntactic_form: [pat_fun, mk_ap_pat([[pat_arg]])],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let conaps: group = {
  id: ApConsPat,
  forms: [conapp_pat],
};

let funaps: group = {
  id: ApFuncPat,
  forms: [funapp_pat],
};
