open Haz3lcore;
open ExplainThisForm;
open Example;

let if_basic1_exp_ex = {
  sub_id: Derive,
  term: mk_example("from [] to []|-truth by rule_Truth_I"),
  message: "The truth rule states that ⊤ is true.",
};

let _exp_prems = exp("e_prems");
let _exp_concl = exp("e_concl");
let _exp_rule = exp("e_rule");
let derive_exp_coloring_ids =
    (~prems_id: Id.t, ~concl_id: Id.t, ~rule_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_prems), prems_id),
  (Piece.id(_exp_concl), concl_id),
  (Piece.id(_exp_rule), rule_id),
];

let derive_exp: form = {
  let explanation = "Dersive from the [*premises*](%s) to the [*conclusion*](%s) by the [*rule*](%s).";
  {
    id: DeriveExp,
    syntactic_form: [
      mk_derive([
        [space(), _exp_prems, linebreak()],
        [space(), _exp_concl, linebreak()],
        [space(), _exp_rule],
      ]),
    ],
    expandable_id: None,
    explanation,
    examples: [if_basic1_exp_ex],
  };
};

let derives: group = {id: DeriveExp, forms: [derive_exp]};
