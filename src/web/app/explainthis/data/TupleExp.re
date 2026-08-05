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

let tuple_example_labeled_1 = {
  sub_id: TupleLabeled1,
  term: mk_example("(x=1)"),
  message: "A labeled tuple with the label x and the element 1.",
};
let tuple_example_labeled_2 = {
  sub_id: TupleLabeled2,
  term: mk_example("(1, y=2)"),
  message: "A tuple with first element 1 and second element 2 with the label y.",
};
let tuple_example_labeled_3 = {
  sub_id: TupleLabeled3,
  term: mk_example("(x=1, 4, y=2)"),
  message: "A tuple with first element 1 labeled with x, second element 4 unlabelled, and third element 2 with label y.",
};

let tuple_exp_id: form_id = TupleExp;
let tuple_exp_comma = comma_exp();
let tuple_exp_form = [exp("e1"), tuple_exp_comma, space(), exp("...")];
let tuple_exp_explanation = (~n: int): string =>
  Printf.sprintf("The tuple has %d elements.", n);
let tuple_exp = (~n: int): form => {
  id: tuple_exp_id,
  syntactic_form: tuple_exp_form,
  expandable_id:
    Some((
      Piece.id(tuple_exp_comma),
      [exp("e1"), comma_exp(), exp("...")],
    )),
  explanation: tuple_exp_explanation(~n),
  examples: [
    tuple_example_1,
    tuple_example_2,
    tuple_example_labeled_1,
    tuple_example_labeled_2,
  ],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let tuple_exp_size2_coloring_ids =
    (~exp1_id: Id.t, ~exp2_id: Id.t): list((Id.t, Id.t)) => {
  [(Piece.id(exp1), exp1_id), (Piece.id(exp2), exp2_id)];
};
let tuple_exp_size2_id: form_id = Tuple2Exp;
let tuple_exp_size2_comma = comma_exp();
let tuple_exp_size2_form = [exp1, tuple_exp_size2_comma, space(), exp2];
let tuple_exp_size2 = (~exp1_id: Id.t, ~exp2_id: Id.t): form => {
  id: tuple_exp_size2_id,
  syntactic_form: tuple_exp_size2_form,
  expandable_id:
    Some((
      Piece.id(tuple_exp_size2_comma),
      [exp("e1"), comma_exp(), exp("e2")],
    )),
  explanation:
    Printf.sprintf(
      "The 2-tuple has a [first](%s) and [second](%s) element.",
      Id.to_string(exp1_id),
      Id.to_string(exp2_id),
    ),
  examples: [tuple_example_1, tuple_example_labeled_2],
};
let exp1 = exp("e1");
let exp2 = exp("e2");
let exp3 = exp("e3");
let tuple_exp_size3_coloring_ids =
    (~exp1_id: Id.t, ~exp2_id: Id.t, ~exp3_id: Id.t): list((Id.t, Id.t)) => {
  [
    (Piece.id(exp1), exp1_id),
    (Piece.id(exp2), exp2_id),
    (Piece.id(exp3), exp3_id),
  ];
};
let tuple_exp_size3_id: form_id = Tuple3Exp;
let tuple_exp_size3_comma = comma_exp();
let tuple_exp_size3_form = [
  exp1,
  comma_exp(),
  space(),
  exp2,
  tuple_exp_size3_comma,
  space(),
  exp3,
];
let tuple_exp_size3 = (~exp1_id: Id.t, ~exp2_id: Id.t, ~exp3_id: Id.t): form => {
  id: tuple_exp_size3_id,
  syntactic_form: tuple_exp_size3_form,
  expandable_id:
    Some((
      Piece.id(tuple_exp_size3_comma),
      [exp("e1"), comma_exp(), exp("e2"), comma_exp(), exp("e3")],
    )),
  explanation:
    Printf.sprintf(
      "The 3-tuple has a [first](%s), [second](%s), and [third](%s) element.",
      Id.to_string(exp1_id),
      Id.to_string(exp2_id),
      Id.to_string(exp3_id),
    ),
  examples: [tuple_example_2, tuple_example_labeled_3],
};

let exp_x = exp("x");
let exp_y = exp("y");
let tuple_extension_exp_coloring_ids =
    (~x_id: Id.t, ~y_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(exp_x), x_id),
  (Piece.id(exp_y), y_id),
];
let tuple_extension_exp_form = [
  exp_x,
  space(),
  tuple_extension_exp(),
  space(),
  exp_y,
];
let tuple_extension_exp = (~x_id: Id.t, ~y_id: Id.t): form => {
  id: TupleExtensionExp,
  syntactic_form: tuple_extension_exp_form,
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "Creates a tuple by combining the [*first operand*](%s) and the [*second operand*](%s), updating elements with the same labels.",
      Id.to_string(x_id),
      Id.to_string(y_id),
    ),
  examples: [
    {
      sub_id: TupleExtension1,
      term: mk_example("(1, 2) ... (3, 4)"),
      message: "Combines the tuples (1, 2) and (3, 4) into a new tuple.",
    },
    {
      sub_id: TupleExtension2,
      term: mk_example("(x=1, y=2) ... (x=3, z=4)"),
      message: "Combines the labeled tuples (x=1, y=2) and (x=3, z=4), updating the x label to 3 and adding a new label z with value 4.",
    },
    {
      sub_id: TupleExtension3,
      term:
        mk_example(
          {|("Alice", active=true, age=30, location="Paris") ... ("Engineer", age=31, department="R&D", active=false)|},
        ),
      message: {|Combines a partially labeled tuple representing a user with another tuple containing new and overlapping fields.
        The `age` and `active` labels are updated, and a new label `department` is added. The unlabeled string "Engineer" is
        added in order after the original unlabeled "Alice".|},
    },
  ],
};

let tuple_extensions = (~x_id: Id.t, ~y_id: Id.t): group => {
  id: TupleExtensionExp,
  forms: [tuple_extension_exp(~x_id, ~y_id)],
};

let tuples = (~n: int): group => {
  id: TupleExp,
  forms: [tuple_exp(~n)],
};

let tuples2 = (~exp1_id: Id.t, ~exp2_id: Id.t, ~n: int): group => {
  id: Tuple2Exp,
  forms: [tuple_exp_size2(~exp1_id, ~exp2_id), tuple_exp(~n)],
};

let tuples3 = (~exp1_id: Id.t, ~exp2_id: Id.t, ~exp3_id: Id.t, ~n: int): group => {
  id: Tuple3Exp,
  forms: [tuple_exp_size3(~exp1_id, ~exp2_id, ~exp3_id), tuple_exp(~n)],
};
