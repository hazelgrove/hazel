open Haz3lcore;
open Example;
open ExplainThisForm;

let tuple_typ_id: form_id = TupleTyp;
let tuple_typ_comma = comma_typ();
let tuple_typ_form = [typ("ty1"), tuple_typ_comma, space(), typ("...")];
let tuple_typ_explanation = (~n: int): string =>
  Printf.sprintf(
    "This tuple type classifies %d-tuples with corresponding element types.",
    n,
  );
let tuple_typ = (~n: int): form => {
  id: tuple_typ_id,
  syntactic_form: tuple_typ_form,
  expandable_id:
    Some((
      Piece.id(tuple_typ_comma),
      [typ("ty1"), comma_typ(), typ("...")],
    )),
  explanation: tuple_typ_explanation(~n),
  examples: [],
};

let tuple0_typ: form = {
  let explanation = "This edge-case tuple type, also known as the `unit` type, classifies 0-tuples, of which there is only one.";
  {
    id: Tuple0Typ,
    syntactic_form: [typ("()")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let typ_elem1 = typ("ty1");
let typ_elem2 = typ("ty2");
let tuple2_typ_coloring_ids =
    (~elem1_id: Id.t, ~elem2_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(typ_elem1), elem1_id),
  (Piece.id(typ_elem2), elem2_id),
];
let tuple2_typ_id: form_id = Tuple2Typ;
let tuple2_typ_comma = comma_typ();
let tuple2_typ_form = [typ_elem1, tuple2_typ_comma, space(), typ_elem2];
let tuple2_typ = (~elem1_id: Id.t, ~elem2_id: Id.t): form => {
  id: tuple2_typ_id,
  syntactic_form: tuple2_typ_form,
  expandable_id:
    Some((
      Piece.id(tuple2_typ_comma),
      [typ("ty1"), comma_typ(), typ("ty2")],
    )),
  explanation:
    Printf.sprintf(
      "This tuple type classifies 2-tuples with the first element of the [first element type](%s) and second element of the [second element type](%s).",
      Id.to_string(elem1_id),
      Id.to_string(elem2_id),
    ),
  examples: [],
};
let typ_elem1 = typ("ty1");
let typ_elem2 = typ("ty2");
let typ_elem3 = typ("ty3");
let tuple3_typ_coloring_ids =
    (~elem1_id: Id.t, ~elem2_id: Id.t, ~elem3_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(typ_elem1), elem1_id),
  (Piece.id(typ_elem2), elem2_id),
  (Piece.id(typ_elem3), elem3_id),
];
let tuple3_typ_id: form_id = Tuple3Typ;
let tuple3_typ_comma = comma_typ();
let tuple3_typ_form = [
  typ_elem1,
  comma_typ(),
  space(),
  typ_elem2,
  tuple3_typ_comma,
  space(),
  typ_elem3,
];
let tuple3_typ = (~elem1_id: Id.t, ~elem2_id: Id.t, ~elem3_id: Id.t): form => {
  id: tuple3_typ_id,
  syntactic_form: tuple3_typ_form,
  expandable_id:
    Some((
      Piece.id(tuple3_typ_comma),
      [typ("ty1"), comma_typ(), typ("ty2"), comma_typ(), typ("ty3")],
    )),
  explanation:
    Printf.sprintf(
      "This tuple type classifies 3-tuples with the first element of the [first element type](%s), second element of the [second element type](%s), and third element of the [third element type](%s).",
      Id.to_string(elem1_id),
      Id.to_string(elem2_id),
      Id.to_string(elem3_id),
    ),
  examples: [],
};

let tuple = (~n: int): group => {
  id: TupleTyp,
  forms: [tuple_typ(~n)],
};

let tuple0: group = {
  id: Tuple0Typ,
  forms: [tuple0_typ],
};

let tuple2 = (~elem1_id: Id.t, ~elem2_id: Id.t, ~n: int): group => {
  id: Tuple2Typ,
  forms: [tuple2_typ(~elem1_id, ~elem2_id), tuple_typ(~n)],
};

let tuple3 =
    (~elem1_id: Id.t, ~elem2_id: Id.t, ~elem3_id: Id.t, ~n: int): group => {
  id: Tuple3Typ,
  forms: [tuple3_typ(~elem1_id, ~elem2_id, ~elem3_id), tuple_typ(~n)],
};
