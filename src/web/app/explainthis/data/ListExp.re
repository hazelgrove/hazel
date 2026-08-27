open Haz3lcore;
open ExplainThisForm;
open Example;

let list_exp = (~n: int): form => {
  let int_list = {
    sub_id: List(Int),
    term: mk_example("[1, 2]"),
    message: "A list with two elements, 1 and 2.",
  };
  let tuple_list = {
    sub_id: List(Tuple),
    term: mk_example("[(1, true), (2, false)]"),
    message: "A list with two elements, a tuple with 1 and true and a tuple with 2 and false.",
  };
  let explanation = Printf.sprintf("List literal with %d element(s).", n);
  {
    id: ListExp,
    syntactic_form: [
      mk_list_exp([[exp("e1"), comma_exp(), space(), exp("...")]]),
    ],
    colorings: [],
    expandable_id: None,
    explanation,
    examples: [int_list, tuple_list],
  };
};

let cons1_ex = {
  sub_id: List(Cons1),
  term: mk_example("1::[]"),
  message: "A single element list of 1.",
};
let cons2_ex = {
  sub_id: List(Cons2),
  term: mk_example("true::false::[]"),
  message: "A list with two elements, true and false.",
};
let exp_hd = exp("e_hd");
let exp_tl = exp("e_tl");
let cons_exp_coloring_ids = (~hd_id: Id.t, ~tl_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(exp_hd), hd_id),
  (Piece.id(exp_tl), tl_id),
];
let cons_exp = (~hd_id: Id.t, ~tl_id: Id.t): form => {
  let explanation =
    Printf.sprintf(
      "Creates a list with [*head element*](%s) and [*tail element*](%s).",
      Id.to_string(hd_id),
      Id.to_string(tl_id),
    );
  {
    id: ConsExp,
    syntactic_form: [exp_hd, cons_exp(), exp_tl],
    colorings: cons_exp_coloring_ids(~hd_id, ~tl_id),
    expandable_id: None,
    explanation,
    examples: [cons1_ex, cons2_ex],
  };
};

let exp_xs = exp("xs");
let exp_ys = exp("ys");
let concat_exp_coloring_ids =
    (~xs_id: Id.t, ~ys_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(exp_xs), xs_id),
  (Piece.id(exp_ys), ys_id),
];
let list_concat_exp = (~xs_id: Id.t, ~ys_id: Id.t): form => {
  let explanation =
    Printf.sprintf(
      "Creates a list by combining the [*first operand*](%s) and the [*second operand*](%s).",
      Id.to_string(xs_id),
      Id.to_string(ys_id),
    );
  {
    id: ListConcatExp,
    syntactic_form: [exp_xs, space(), list_concat_exp(), space(), exp_ys],
    colorings: concat_exp_coloring_ids(~xs_id, ~ys_id),
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let listlits = (~n: int): group => singleton(list_exp(~n));

let listcons = (~hd_id: Id.t, ~tl_id: Id.t): group =>
  singleton(cons_exp(~hd_id, ~tl_id));

let listconcats = (~xs_id: Id.t, ~ys_id: Id.t): group =>
  singleton(list_concat_exp(~xs_id, ~ys_id));
