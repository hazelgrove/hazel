open Haz3lcore;
open Example;
open ExplainThisForm;

let _pat_con = pat("p_con");
let _pat_arg = pat("p_arg");
let ap_pat_coloring_ids = (~con_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat_con), con_id),
  (Piece.id(_pat_arg), arg_id),
];
let ap_pat: form = {
  let explanation = "Only expressions that match the [*constructor*](%s) with an *argument* matching the [*argument pattern*](%s) match this *constructor application pattern*.";
  {
    id: ApPat,
    syntactic_form: [_pat_con, mk_ap_pat([[_pat_arg]])],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let ap: group = {id: ApPat, forms: [ap_pat]};
