open Haz3lcore;
open Example;
open ExplainThisForm;

let p = pat("p");
let exp_thm = exp("h");
let _proof = proof("?");
let exp_body = exp("e");
let test_exp_coloring_ids =
    (~body_id: Id.t, ~pat_id: Id.t, ~thm_id: Id.t, ~proof_id: Id.t)
    : list((Id.t, Id.t)) => [
  (Piece.id(p), pat_id),
  (Piece.id(exp_thm), thm_id),
  (Piece.id(_proof), proof_id),
  (Piece.id(exp_body), body_id),
];
let theorem_exp: form = {
  let explanation = "Asserts that the [*statement*](%s) denotes `true`, checked by the following [*proof*](%s), and binds the [*name*](%s) in the theorem namespace, where later proofs can cite it as a rule. Theorem names are not values: they have no type, they never evaluate, and they shadow only each other. The [*body*](%s) is ordinary code, so using the theorem's name there is an unbound variable.";
  {
    id: TheoremExp,
    syntactic_form: [
      mk_theorem([
        [space(), p, space()],
        [space(), exp_thm, space()],
        [space(), _proof, space()],
      ]),
      linebreak(),
      exp_body,
    ],
    expandable_id: None,
    explanation,
    examples: [
      {
        sub_id: Theorem,
        term:
          mk_example(
            "theorem lem = 1 + 1 == 2 proof eval 1 + 1 at 0 end; axiom refl_eq at 0 on 2 == 2 end in 0",
          ),
        message: "The statement `1 + 1 == 2` is proven by evaluating the sum and closing the resulting `2 == 2` by reflexivity. `lem` is now citable in the proofs of later theorems; the body is the ordinary expression `0`.",
      },
    ],
  };
};

let tests: group = {
  id: TheoremExp,
  forms: [theorem_exp],
};
