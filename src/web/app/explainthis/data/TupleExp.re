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

let tuple_exp: form = {
  let explanation = "The tuple has %s elements.";
  let comma = comma_exp();
  {
    id: TupleExp,
    syntactic_form: [exp("e1"), comma, space(), exp("...")],
    expandable_id:
      Some((Piece.id(comma), [exp("e1"), comma_exp(), exp("...")])),
    explanation,
    examples: [
      tuple_example_1,
      tuple_example_2,
      tuple_example_labeled_1,
      tuple_example_labeled_2,
    ],
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
    examples: [tuple_example_1, tuple_example_labeled_2],
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
    examples: [tuple_example_2, tuple_example_labeled_3],
  };
};

let _exp_x = exp("x");
let _exp_y = exp("y");
let tuple_extension_exp_coloring_ids =
    (~x_id: Id.t, ~y_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_x), x_id),
  (Piece.id(_exp_y), y_id),
];
let tuple_extension_exp: form = {
  let explanation = "Creates a tuple by combining the [*first operand*](%s) and the [*second operand*](%s), updating elements with the same labels.";
  {
    id: TupleExtensionExp,
    syntactic_form: [_exp_x, space(), tuple_extension_exp(), space(), _exp_y],
    expandable_id: None,
    explanation,
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
};

let tuple_extensions: group = {
  id: TupleExtensionExp,
  forms: [tuple_extension_exp],
};

let tuples: group = {
  id: TupleExp,
  forms: [tuple_exp],
};

let tuples2: group = {
  id: Tuple2Exp,
  forms: [tuple_exp_size2, tuple_exp],
};

let tuples3: group = {
  id: Tuple3Exp,
  forms: [tuple_exp_size3, tuple_exp],
};
