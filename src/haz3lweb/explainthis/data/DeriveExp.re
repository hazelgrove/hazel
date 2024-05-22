open Haz3lcore;
open ExplainThisForm;
open Example;

let if_basic1_exp_ex = {
  sub_id: Derive,
  term: mk_example("der []|-truth \\ [] of rule_Truth_I"),
  message: "The truth rule states that ⊤ is true.",
};

let _exp_concl = exp("e_concl");
let _exp_rule = exp("e_rule");
let _exp_prem = exp("e_prem");
let derive_exp_coloring_ids =
    (~concl_id: Id.t, ~prems_id: Id.t, ~rule_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_concl), concl_id),
  (Piece.id(_exp_rule), rule_id),
  (Piece.id(_exp_prem), prems_id),
];

let derive_exp: form = {
  let explanation = "To derive the [*conclusion*](%s), use the [*rule*](%s) with the [*premises*](%s).";
  {
    id: DeriveExp,
    syntactic_form: [
      mk_derive([
        [space(), _exp_concl, linebreak()],
        [space(), _exp_rule, linebreak()],
      ]),
      space(),
      _exp_prem,
    ],
    expandable_id: None,
    explanation,
    examples: [if_basic1_exp_ex],
  };
};

let derives: group = {id: DeriveExp, forms: [derive_exp]};
