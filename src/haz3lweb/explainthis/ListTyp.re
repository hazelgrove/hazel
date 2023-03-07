/*open Haz3lcore;
  open ExampleUtil;
  open ExplainThisForm;

  let list_typ_group = "list_typ_group";
  let _typ_elem = typ("ty_elem");
  // TODO Syntactic form coloring looks off for this one and other types ones...
  let list_typ_coloring_ids = (~elem_id: Id.t): list((Id.t, Id.t)) => [
    (Piece.id(_typ_elem), elem_id),
  ];
  let list_typ: form = {
    let explanation = {
      message: "List type. The list type classifies lists with elements with the corresponding [*element type*](%i).",
      feedback: Unselected,
    };
    {
      id: "list_typ",
      syntactic_form: [mk_list_typ([[_typ_elem]])],
      expandable_id: None,
      explanation,
      examples: [],
    };
  };*/
