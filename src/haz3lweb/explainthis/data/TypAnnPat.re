/*open Haz3lcore;
  open Example;
  open ExplainThisForm;
  let typann_pat_group = "typann_pat_group";
  let _pat = pat("p");
  let _typ = typ("ty");
  let typann_pat_coloring_ids =
      (~pat_id: Id.t, ~typ_id: Id.t): list((Id.t, Id.t)) => [
    (Piece.id(_pat), pat_id),
    (Piece.id(_typ), typ_id),
  ];
  let typann_pat: form = {
    let explanation = {
      message: "Type annotation pattern. Only expressions that match the [type annotated pattern](%i) and have the [indicated type](%i) match this type annotation pattern.",
      feedback: Unselected,
    };
    {
      id: "typann_pat",
      syntactic_form: [_pat, space(), typeann(), space(), _typ],
      expandable_id: None,
      explanation,
      examples: [],
    };
  };*/
