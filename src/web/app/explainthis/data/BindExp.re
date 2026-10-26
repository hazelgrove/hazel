open Haz3lcore;
open Example;
open ExplainThisForm;

/* `do p <- c in body` -- Figure 3's monadic bind, as syntax.

   Deliberately explained as sequencing rather than as binding: the thing
   that makes it not a `let` is that `c` is a COMMAND, which the system
   performs, and `p` names the answer it comes back with. */

let bind_basic_exp_ex = {
  sub_id: BindBasic,
  term: mk_example("do r <- new_splice(IntT, None) in return(r)"),
  message: "A splice is created, the reference it answers with is bound to r, and the rest of the sequence can use it.",
};

let exp_c = exp("c");
let exp_body = exp("body");
let pat_p = pat("p");

let bind_exp_coloring_ids =
    (~pat_id: Id.t, ~cmd_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(pat_p), pat_id),
  (Piece.id(exp_c), cmd_id),
];

let bind_exp_form = [
  mk_bind([[space(), pat_p, space()], [space(), exp_c, space()]]),
  space(),
  exp_body,
];

let bind_exp = (~pat_id: Id.t, ~cmd_id: Id.t): form => {
  id: BindExp,
  syntactic_form: bind_exp_form,
  colorings: bind_exp_coloring_ids(~pat_id, ~cmd_id),
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "The [command](%s) is performed by the system, its answer is bound to the [pattern](%s), and the rest of the sequence runs with that binding. The pattern does not reach the command itself, so a bind is never recursive.",
      Id.to_string(cmd_id),
      Id.to_string(pat_id),
    ),
  examples: [bind_basic_exp_ex],
};

let binds = (~pat_id: Id.t, ~cmd_id: Id.t): group =>
  singleton(bind_exp(~pat_id, ~cmd_id));
