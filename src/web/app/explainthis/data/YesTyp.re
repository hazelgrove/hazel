open Haz3lcore;
open Example;
open ExplainThisForm;

let _exp = exp("exp");
let yes_typ_coloring_ids = (~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp), body_id),
];
let yes_typ: form = {
  let explanation = "This type asserts that the [*enclosed boolean*](%s) is in fact true.";
  {
    id: YesTyp,
    syntactic_form: [mk_yes([[space(), _exp, space()]])],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let yes: group = {
  id: YesTyp,
  forms: [yes_typ],
};
