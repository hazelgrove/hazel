open Haz3lcore;
open Example;
open ExplainThisForm;

let tuple_pat_id: form_id = TuplePat;
let tuple_pat_comma = comma_pat();
let tuple_pat_form = [pat("p1"), tuple_pat_comma, space(), pat("...")];
let tuple_pat_explanation = (~n: int): string =>
  Printf.sprintf(
    "Only expressions that are %d-tuples with elements matching the corresponding element patterns match this tuple pattern.",
    n,
  );
let tuple_pat = (~n: int): form => {
  id: tuple_pat_id,
  syntactic_form: tuple_pat_form,
  colorings: [],
  expandable_id:
    Some((
      Piece.id(tuple_pat_comma),
      [pat("p1"), comma_pat(), pat("...")],
    )),
  explanation: tuple_pat_explanation(~n),
  examples: [],
};
let pat1 = pat("p1");
let pat2 = pat("p2");
let tuple_pat_size2_coloring_ids =
    (~elem1_id: Id.t, ~elem2_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(pat1), elem1_id),
  (Piece.id(pat2), elem2_id),
];
let tuple_pat_size2_id: form_id = Tuple2Pat;
let tuple_pat_size2_comma = comma_pat();
let tuple_pat_size2_form = [pat1, tuple_pat_size2_comma, space(), pat2];
let tuple_pat_size2 = (~elem1_id: Id.t, ~elem2_id: Id.t): form => {
  id: tuple_pat_size2_id,
  syntactic_form: tuple_pat_size2_form,
  colorings: tuple_pat_size2_coloring_ids(~elem1_id, ~elem2_id),
  expandable_id:
    Some((
      Piece.id(tuple_pat_size2_comma),
      [pat("p1"), comma_pat(), pat("p2")],
    )),
  explanation:
    Printf.sprintf(
      "Only expressions that are 2-tuples with first element matching the [first element pattern](%s) and second element matching the [second element pattern](%s) match this tuple pattern.",
      Id.to_string(elem1_id),
      Id.to_string(elem2_id),
    ),
  examples: [],
};
let pat1 = pat("p1");
let pat2 = pat("p2");
let pat3 = pat("p3");
let tuple_pat_size3_coloring_ids =
    (~elem1_id: Id.t, ~elem2_id: Id.t, ~elem3_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(pat1), elem1_id),
  (Piece.id(pat2), elem2_id),
  (Piece.id(pat3), elem3_id),
];
let tuple_pat_size3_id: form_id = Tuple3Pat;
let tuple_pat_size3_comma = comma_pat();
let tuple_pat_size3_form = [
  pat1,
  comma_pat(),
  space(),
  pat2,
  tuple_pat_size3_comma,
  space(),
  pat3,
];
let tuple_pat_size3 =
    (~elem1_id: Id.t, ~elem2_id: Id.t, ~elem3_id: Id.t): form => {
  id: tuple_pat_size3_id,
  syntactic_form: tuple_pat_size3_form,
  colorings: tuple_pat_size3_coloring_ids(~elem1_id, ~elem2_id, ~elem3_id),
  expandable_id:
    Some((
      Piece.id(tuple_pat_size3_comma),
      [pat("p1"), comma_pat(), pat("p2"), comma_pat(), pat("p3")],
    )),
  explanation:
    Printf.sprintf(
      "Only expressions that are 3-tuples with first element matching the [first element pattern](%s), second element matching the [second element pattern](%s), and third element matching the [third element pattern](%s) match this tuple pattern.",
      Id.to_string(elem1_id),
      Id.to_string(elem2_id),
      Id.to_string(elem3_id),
    ),
  examples: [],
};

let tuple = (~n: int): group => singleton(tuple_pat(~n));
let tuple2 = (~elem1_id: Id.t, ~elem2_id: Id.t, ~n: int): group => {
  id: Tuple2Pat,
  forms: [tuple_pat_size2(~elem1_id, ~elem2_id), tuple_pat(~n)],
};
let tuple3 =
    (~elem1_id: Id.t, ~elem2_id: Id.t, ~elem3_id: Id.t, ~n: int): group => {
  id: Tuple3Pat,
  forms: [tuple_pat_size3(~elem1_id, ~elem2_id, ~elem3_id), tuple_pat(~n)],
};
