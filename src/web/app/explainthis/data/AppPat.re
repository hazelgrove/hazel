open Haz3lcore;
open Example;
open ExplainThisForm;

let _pat_con = pat("p_con");
let _pat_arg = pat("p_arg");
let conapp_pat_coloring_ids =
    (~x_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat_con), x_id),
  (Piece.id(_pat_arg), arg_id),
];
let conapp_pat: form = {
  let explanation = "Only expressions that match the [*constructor*](%s) with an *argument* matching the [*argument pattern*](%s) match this *constructor application pattern*.";
  {
    id: ApConsPat,
    syntactic_form: [_pat_con, mk_ap_pat([[_pat_arg]])],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let _pat_fun = pat("p_fun");
let _pat_arg = pat("p_arg");
let funapp_pat_coloring_ids =
    (~x_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat_fun), x_id),
  (Piece.id(_pat_arg), arg_id),
];
let funapp_pat: form = {
  let explanation = "Defines a function [*function*](%s) with [*arguments*](%s) through this *function application pattern*.";
  {
    id: ApConsPat,
    syntactic_form: [_pat_fun, mk_ap_pat([[_pat_arg]])],
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
