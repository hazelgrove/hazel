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
let exp_cond = exp("e_cond");
let exp_then = exp("e_then");
let exp_else = exp("e_else");
let if_exp_coloring_ids =
    (~cond_id: Id.t, ~then_id: Id.t, ~else_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(exp_cond), cond_id),
  (Piece.id(exp_then), then_id),
  (Piece.id(exp_else), else_id),
];
let if_exp_form = [
  mk_if([
    [space(), exp_cond, linebreak()],
    [space(), exp_then, linebreak()],
  ]),
  space(),
  exp_else,
];
let if_exp = (~cond_id: Id.t, ~then_id: Id.t, ~else_id: Id.t): form => {
  id: IfExp,
  syntactic_form: if_exp_form,
  colorings: [],
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "If the [*condition*](%s) evaluates to `true`, evaluate the [*then branch*](%s). Otherwise, evaluate the [*else branch*](%s).",
      Id.to_string(cond_id),
      Id.to_string(then_id),
      Id.to_string(else_id),
    ),
  examples: [if_basic1_exp_ex, if_basic2_exp_ex],
};

let ifs = (~cond_id: Id.t, ~then_id: Id.t, ~else_id: Id.t): group => {
  id: IfExp,
  forms: [if_exp(~cond_id, ~then_id, ~else_id)],
};
