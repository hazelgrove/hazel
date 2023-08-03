open Haz3lcore;
open ExplainThisForm;
open Example;

let if_basic1_exp_ex = {
  sub_id: IfTrue,
  term: mk_example("if (true) then 1 else 2"),
  message: "Since the condition is true, the if expression evaluates to the then branch, 1.",
};
let if_basic2_exp_ex = {
  sub_id: IfFalse,
  term: mk_example("if (2 < 1) then 3 else 4"),
  message: "Since the condition is 2 < 1 is false, the if expression evaluates to the else branch, 4.",
};
let _exp_cond = exp("e_cond");
let _exp_then = exp("e_then");
let _exp_else = exp("e_else");
let if_exp_coloring_ids =
    (~cond_id: Id.t, ~then_id: Id.t, ~else_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_cond), cond_id),
  (Piece.id(_exp_then), then_id),
  (Piece.id(_exp_else), else_id),
];
let if_exp: form = {
  let explanation = "If expression. If the [*condition*](%i) evaluates to `true`, evaluate the [*then branch*](%i). Otherwise, evaluate the [*else branch*](%i).";
  {
    id: IfExp,
    syntactic_form: [
      mk_if([
        [space(), _exp_cond, linebreak()],
        [space(), _exp_then, linebreak()],
      ]),
      space(),
      _exp_else,
    ],
    expandable_id: None,
    explanation,
    examples: [if_basic1_exp_ex, if_basic2_exp_ex],
  };
};

let ifs: group = {id: IfExp, forms: [if_exp]};
