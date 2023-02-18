open Haz3lcore;
open ExampleUtil;
open ExplainThisForm;

let tuple_pat_group = "tuple_pat_group";
let tuple_pat_2_group = "tuple_pat_2_group";
let tuple_pat_3_group = "tuple_pat_3_group";
let tuple_pat: form = {
  let explanation = {
    message: "Tuple pattern. Only expressions that are %i-tuples with elements matching the corresponding element patterns match this tuple pattern.",
    feedback: Unselected,
  };
  let comma = comma_pat();
  {
    id: "tuple_pat",
    syntactic_form: [pat("p1"), comma, space(), pat("...")],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [],
  };
};
let _pat1 = pat("p1");
let _pat2 = pat("p2");
let tuple_pat_size2_coloring_ids =
    (~elem1_id: Id.t, ~elem2_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat1), elem1_id),
  (Piece.id(_pat2), elem2_id),
];
let tuple_pat_size2: form = {
  let explanation = {
    message: "Tuple pattern. Only expressions that are 2-tuples with first element matching the [first element pattern](%i) and second element matching the [second element pattern](%i) match this tuple pattern.",
    feedback: Unselected,
  };
  let comma = comma_pat();
  {
    id: "tuple_pat_size2",
    syntactic_form: [_pat1, comma, space(), _pat2],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [],
  };
};
let _pat1 = pat("p1");
let _pat2 = pat("p2");
let _pat3 = pat("p3");
let tuple_pat_size3_coloring_ids =
    (~elem1_id: Id.t, ~elem2_id: Id.t, ~elem3_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat1), elem1_id),
  (Piece.id(_pat2), elem2_id),
  (Piece.id(_pat3), elem3_id),
];
let tuple_pat_size3: form = {
  let explanation = {
    message: "Tuple pattern. Only expressions that are 3-tuples with first element matching the [first element pattern](%i), second element matching the [second element pattern](%i), and third element matching the [third element pattern](%i) match this tuple pattern.",
    feedback: Unselected,
  };
  let comma = comma_pat();
  {
    id: "tuple_pat_size3",
    syntactic_form: [
      _pat1,
      comma_pat(),
      space(),
      _pat2,
      comma,
      space(),
      _pat3,
    ],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [],
  };
};
