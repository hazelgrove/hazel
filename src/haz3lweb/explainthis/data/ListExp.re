open Haz3lcore;
open ExplainThisForm;
open Example;

let list_exp: form = {
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
  let explanation = "List literal with %s element(s).";
  {
    id: ListExp,
    syntactic_form: [
      mk_list_exp([[exp("e1"), comma_exp(), space(), exp("...")]]),
    ],
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
let _exp_hd = exp("e_hd");
let _exp_tl = exp("e_tl");
let cons_exp_coloring_ids = (~hd_id: Id.t, ~tl_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_hd), hd_id),
  (Piece.id(_exp_tl), tl_id),
];
let cons_exp: form = {
  let explanation = "Cons operator. Creates a list with [*head element*](%s) and [*tail element*](%s).";
  {
    id: ConsExp,
    syntactic_form: [_exp_hd, cons_exp(), _exp_tl],
    expandable_id: None,
    explanation,
    examples: [cons1_ex, cons2_ex],
  };
};

let _exp_xs = exp("xs");
let _exp_ys = exp("ys");
let concat_exp_coloring_ids =
    (~xs_id: Id.t, ~ys_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_xs), xs_id),
  (Piece.id(_exp_ys), ys_id),
];
let list_concat_exp: form = {
  let explanation = "List concatenation operator. Creates a list by combining the [*first operand*](%s) and the [*second operand*](%s).";
  {
    id: ListConcatExp,
    syntactic_form: [_exp_xs, space(), list_concat_exp(), space(), _exp_ys],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let listlits: group = {id: ListExp, forms: [list_exp]};

let listcons: group = {id: ConsExp, forms: [cons_exp]};

let listconcats: group = {id: ListConcatExp, forms: [list_concat_exp]};
