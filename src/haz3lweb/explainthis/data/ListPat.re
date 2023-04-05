/*open Haz3lcore;
  open Example;
  open ExplainThisForm;

  let listlit_pat_group = "listlit_pat_group";
  let listlit_pat: form = {
    let explanation = {
      message: "List literal pattern. Only expressions that are lists with %i-elements where each element matches the corresponding element pattern match this *list literal pattern*.",
      feedback: Unselected,
    };
    {
      id: "listlit_pat",
      syntactic_form: [
        mk_list_pat([[pat("p1"), comma_pat(), space(), pat("...")]]),
      ],
      expandable_id: None,
      explanation,
      examples: [],
    };
  };
  let listnil_pat_group = "listnil_pat_group";
  let listnil_pat: form = {
    let explanation = {
      message: "Empty list pattern. Only expressions that are empty lists `nil` match the *empty list `nil` pattern*.",
      feedback: Unselected,
    };
    {
      id: "listnil_pat",
      syntactic_form: [pat("nil")],
      expandable_id: None,
      explanation,
      examples: [],
    };
  };

  let cons_pat_group = "cons_pat_group";
  let cons2_pat_group = "cons2_pat_group";
  let _pat_hd = pat("p_hd");
  let _pat_tl = pat("p_tl");
  let cons_base_pat_coloring_ids =
      (~hd_id: Id.t, ~tl_id: Id.t): list((Id.t, Id.t)) => [
    (Piece.id(_pat_hd), hd_id),
    (Piece.id(_pat_tl), tl_id),
  ];
  let cons_base_pat: form = {
    let explanation = {
      message: "Non-empty list pattern. Only expressions that are non-empty lists with *head element* matching the [*head element pattern*](%i) and *tail* list matching the [*tail pattern*](%i) match this non-empty list pattern.",
      feedback: Unselected,
    };
    {
      id: "cons_base_pat",
      syntactic_form: [_pat_hd, cons_pat(), _pat_tl],
      expandable_id: Some(Piece.id(_pat_tl)),
      explanation,
      examples: [],
    };
  };
  let _pat_fst = pat("p_fst");
  let _pat_snd = pat("p_snd");
  let _pat_tl = pat("p_tl");
  let cons2_pat_coloring_ids =
      (~fst_id: Id.t, ~snd_id: Id.t, ~tl_id: Id.t): list((Id.t, Id.t)) => [
    (Piece.id(_pat_fst), fst_id),
    (Piece.id(_pat_snd), snd_id),
    (Piece.id(_pat_tl), tl_id),
  ];
  let cons2_pat: form = {
    let explanation = {
      message: "Non-empty list pattern. Only expressions that are non-empty lists with *first element* matching the [*first element pattern*](%i), *second element* matching the [*second element pattern*](%i), and *tail* list matching the [*tail pattern*](%i) match this non-empty list pattern.",
      feedback: Unselected,
    };
    let c = cons_pat();
    {
      id: "cons2_pat",
      syntactic_form: [_pat_fst, cons_pat(), _pat_snd, c, _pat_tl],
      expandable_id: Some(Piece.id(c)),
      explanation,
      examples: [],
    };
  };*/
