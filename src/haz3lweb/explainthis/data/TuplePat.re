open Haz3lcore;
open Example;
open ExplainThisForm;

let tuple_pat: form = {
  let explanation = "Only expressions that are %s-tuples with elements matching the corresponding element patterns match this tuple pattern.";
  let comma = comma_pat();
  {
    id: TuplePat,
    syntactic_form: [pat("p1"), comma, space(), pat("...")],
    expandable_id:
      Some((Piece.id(comma), [pat("p1"), comma_pat(), pat("...")])),
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
  let explanation = "Only expressions that are 2-tuples with first element matching the [first element pattern](%s) and second element matching the [second element pattern](%s) match this tuple pattern.";
  let comma = comma_pat();
  {
    id: Tuple2Pat,
    syntactic_form: [_pat1, comma, space(), _pat2],
    expandable_id:
      Some((Piece.id(comma), [pat("p1"), comma_pat(), pat("p2")])),
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
  let explanation = "Only expressions that are 3-tuples with first element matching the [first element pattern](%s), second element matching the [second element pattern](%s), and third element matching the [third element pattern](%s) match this tuple pattern.";
  let comma = comma_pat();
  {
    id: Tuple3Pat,
    syntactic_form: [
      _pat1,
      comma_pat(),
      space(),
      _pat2,
      comma,
      space(),
      _pat3,
    ],
    expandable_id:
      Some((
        Piece.id(comma),
        [pat("p1"), comma_pat(), pat("p2"), comma_pat(), pat("p3")],
      )),
    explanation,
    examples: [],
  };
};

let tuple: group = {id: TuplePat, forms: [tuple_pat]};
let tuple2: group = {id: Tuple2Pat, forms: [tuple_pat_size2, tuple_pat]};
let tuple3: group = {id: Tuple3Pat, forms: [tuple_pat_size3, tuple_pat]};
