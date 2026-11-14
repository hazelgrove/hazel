open Haz3lcore;
open Example;
open ExplainThisForm;

let p = pat("p");
let exp_thm = exp("h");
let pf = proof("?");
let exp_body = exp("e");
let theorem_exp_coloring_ids =
    (~body_id: Id.t, ~pat_id: Id.t, ~thm_id: Id.t, ~proof_id: Id.t)
    : list((Id.t, Id.t)) => [
  (Piece.id(p), pat_id),
  (Piece.id(exp_thm), thm_id),
  (Piece.id(pf), proof_id),
  (Piece.id(exp_body), body_id),
];
let theorem_exp_form = [
  mk_theorem([
    [space(), p, space()],
    [space(), exp_thm, space()],
    [space(), pf, space()],
  ]),
  linebreak(),
  exp_body,
];
let theorem_exp =
    (~pat_id: Id.t, ~thm_id: Id.t, ~proof_id: Id.t, ~body_id: Id.t): form => {
  id: TheoremExp,
  syntactic_form: theorem_exp_form,
  colorings: theorem_exp_coloring_ids(~body_id, ~pat_id, ~thm_id, ~proof_id),
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "Asserts that the [*statement*](%s) denotes `true`, checked by the following [*proof*](%s), and binds the [*name*](%s) in the theorem namespace, where later proofs can cite it as a rule. Theorem names are not values: they have no type, they never evaluate, and they shadow only each other. The [*body*](%s) is ordinary code, so using the theorem's name there is an unbound variable.",
      Id.to_string(thm_id),
      Id.to_string(proof_id),
      Id.to_string(pat_id),
      Id.to_string(body_id),
    ),
  examples: [
    {
      sub_id: Theorem,
      term:
        mk_example(
          "theorem lem = 1 + 1 == 2 proof from 1 + 1 do step end; from 2 == 2 do rewrite refl end end in 0",
        ),
      message: "The statement `1 + 1 == 2` is proven by evaluating the sum and closing the resulting `2 == 2` by reflexivity. `lem` is now citable in the proofs of later theorems; the body is the ordinary expression `0`.",
    },
  ],
};

let tests =
    (~pat_id: Id.t, ~thm_id: Id.t, ~proof_id: Id.t, ~body_id: Id.t): group =>
  singleton(theorem_exp(~pat_id, ~thm_id, ~proof_id, ~body_id));
