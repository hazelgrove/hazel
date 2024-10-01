open Haz3lcore;
open ExplainThisForm;
open Example;

let tuple_example_1 = {
  sub_id: Tuple1,
  term: mk_example("(true, 1)"),
  message: "A tuple with first elment true and second element 1.",
};
let tuple_example_2 = {
  sub_id: Tuple2,
  term: mk_example("(1, 2, 3)"),
  message: "A tuple with first element 1, second element 2, and third element 3.",
};
let tuple_exp: form = {
  let explanation = "The tuple has %s elements.";
  let comma = comma_exp();
  {
    id: TupleExp,
    syntactic_form: [exp("e1"), comma, space(), exp("...")],
    expandable_id:
      Some((Piece.id(comma), [exp("e1"), comma_exp(), exp("...")])),
    explanation,
    examples: [tuple_example_1, tuple_example_2],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let tuple_exp_size2_coloring_ids =
    (~exp1_id: Id.t, ~exp2_id: Id.t): list((Id.t, Id.t)) => {
  [(Piece.id(_exp1), exp1_id), (Piece.id(_exp2), exp2_id)];
};
let tuple_exp_size2: form = {
  let explanation = "The 2-tuple has a [first](%s) and [second](%s) element.";
  let comma = comma_exp();
  {
    id: Tuple2Exp,
    syntactic_form: [_exp1, comma, space(), _exp2],
    expandable_id:
      Some((Piece.id(comma), [exp("e1"), comma_exp(), exp("e2")])),
    explanation,
    examples: [tuple_example_1],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let _exp3 = exp("e3");
let tuple_exp_size3_coloring_ids =
    (~exp1_id: Id.t, ~exp2_id: Id.t, ~exp3_id: Id.t): list((Id.t, Id.t)) => {
  [
    (Piece.id(_exp1), exp1_id),
    (Piece.id(_exp2), exp2_id),
    (Piece.id(_exp3), exp3_id),
  ];
};
let tuple_exp_size3: form = {
  let explanation = "The 3-tuple has a [first](%s), [second](%s), and [third](%s) element.";
  let comma = comma_exp();
  {
    id: Tuple3Exp,
    syntactic_form: [
      _exp1,
      comma_exp(),
      space(),
      _exp2,
      comma,
      space(),
      _exp3,
    ],
    expandable_id:
      Some((
        Piece.id(comma),
        [exp("e1"), comma_exp(), exp("e2"), comma_exp(), exp("e3")],
      )),
    explanation,
    examples: [tuple_example_2],
  };
};

let tuples: group = {id: TupleExp, forms: [tuple_exp]};

let tuples2: group = {id: Tuple2Exp, forms: [tuple_exp_size2, tuple_exp]};

let tuples3: group = {id: Tuple3Exp, forms: [tuple_exp_size3, tuple_exp]};
