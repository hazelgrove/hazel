open Haz3lcore;
open Example;
open ExplainThisForm;

let tuple_typ: form = {
  let explanation = "This tuple type classifies %s-tuples with corresponding element types.";
  let comma = comma_typ();
  {
    id: TupleTyp,
    syntactic_form: [typ("ty1"), comma, space(), typ("...")],
    expandable_id:
      Some((Piece.id(comma), [typ("ty1"), comma_typ(), typ("...")])),
    explanation,
    examples: [],
  };
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

let _typ_elem1 = typ("ty1");
let _typ_elem2 = typ("ty2");
let tuple2_typ_coloring_ids =
    (~elem1_id: Id.t, ~elem2_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_typ_elem1), elem1_id),
  (Piece.id(_typ_elem2), elem2_id),
];
let tuple2_typ: form = {
  let explanation = "This tuple type classifies 2-tuples with the first element of the [first element type](%s) and second element of the [second element type](%s).";
  let comma = comma_typ();
  {
    id: Tuple2Typ,
    syntactic_form: [_typ_elem1, comma, space(), _typ_elem2],
    expandable_id:
      Some((Piece.id(comma), [typ("ty1"), comma_typ(), typ("ty2")])),
    explanation,
    examples: [],
  };
};
let _typ_elem1 = typ("ty1");
let _typ_elem2 = typ("ty2");
let _typ_elem3 = typ("ty3");
let tuple3_typ_coloring_ids =
    (~elem1_id: Id.t, ~elem2_id: Id.t, ~elem3_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_typ_elem1), elem1_id),
  (Piece.id(_typ_elem2), elem2_id),
  (Piece.id(_typ_elem3), elem3_id),
];
let tuple3_typ: form = {
  let explanation = "This tuple type classifies 3-tuples with the first element of the [first element type](%s), second element of the [second element type](%s), and third element of the [third element type](%s).";
  let comma = comma_typ();
  {
    id: Tuple3Typ,
    syntactic_form: [
      _typ_elem1,
      comma_typ(),
      space(),
      _typ_elem2,
      comma,
      space(),
      _typ_elem3,
    ],
    expandable_id:
      Some((
        Piece.id(comma),
        [typ("ty1"), comma_typ(), typ("ty2"), comma_typ(), typ("ty3")],
      )),
    explanation,
    examples: [],
  };
};

let tuple: group = {id: TupleTyp, forms: [tuple_typ]};

let tuple0: group = {id: Tuple0Typ, forms: [tuple0_typ]};

let tuple2: group = {id: Tuple2Typ, forms: [tuple2_typ, tuple_typ]};

let tuple3: group = {id: Tuple3Typ, forms: [tuple3_typ, tuple_typ]};
