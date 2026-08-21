open Haz3lcore;
open Example;
open ExplainThisForm;

let p = pat("p");
let exp_thm = exp("h");
let exp_body = exp("e");
let theorem_exp_coloring_ids =
    (~body_id: Id.t, ~pat_id: Id.t, ~thm_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(p), pat_id),
  (Piece.id(exp_thm), thm_id),
  (Piece.id(exp_body), body_id),
];
let theorem_exp_form = [
  mk_theorem([[space(), p, space()], [space(), exp_thm, space()]]),
  linebreak(),
  exp_body,
];
/* `body_id` is not mentioned in the explanation, but the abstract form has an
   `e` placeholder for the body, so it is needed to colour it. */
let theorem_exp = (~pat_id: Id.t, ~thm_id: Id.t, ~body_id: Id.t): form => {
  id: TheoremExp,
  syntactic_form: theorem_exp_form,
  colorings: theorem_exp_coloring_ids(~body_id, ~pat_id, ~thm_id),
  expandable_id: None,
  explanation:
    Stdlib.Printf.sprintf(
      "Asserts that the [*goal*](%s) is true in the following expression, and [*names*](%s) the theorem for later reuse.",
      Id.to_string(pat_id),
      Id.to_string(thm_id),
    ),
  examples: [],
};

let tests = (~pat_id: Id.t, ~thm_id: Id.t, ~body_id: Id.t): group =>
  singleton(theorem_exp(~pat_id, ~thm_id, ~body_id));
