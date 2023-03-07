/*open Haz3lcore;
  open ExampleUtil;
  open ExplainThisForm;

  let tuple_typ_group = "tuple_typ_group";
  let tuple2_typ_group = "tuple2_typ_group";
  let tuple3_typ_group = "tuple3_typ_group";
  let tuple_typ: form = {
    let explanation = {
      message: "Tuple type. This tuple type classifies %i-tuples with corresponding element types.",
      feedback: Unselected,
    };
    let comma = comma_typ();
    {
      id: "tuple_typ",
      syntactic_form: [typ("ty1"), comma, space(), typ("...")],
      expandable_id: Some(Piece.id(comma)),
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
    let explanation = {
      message: "Tuple type. This tuple type classifies 2-tuples with the first element of the [first element type](%i) and second element of the [second element type](%i).",
      feedback: Unselected,
    };
    let comma = comma_typ();
    {
      id: "tuple2_typ",
      syntactic_form: [_typ_elem1, comma, space(), _typ_elem2],
      expandable_id: Some(Piece.id(comma)),
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
    let explanation = {
      message: "Tuple type. This tuple type classifies 3-tuples with the first element of the [first element type](%i), second element of the [second element type](%i), and third element of the [third element type](%i).",
      feedback: Unselected,
    };
    let comma = comma_typ();
    {
      id: "tuple3_typ",
      syntactic_form: [
        _typ_elem1,
        comma_typ(),
        space(),
        _typ_elem2,
        comma,
        space(),
        _typ_elem3,
      ],
      expandable_id: Some(Piece.id(comma)),
      explanation,
      examples: [],
    };
  };*/
