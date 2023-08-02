open Haz3lcore;
open Example;
open ExplainThisForm;

let listlit_pat: form = {
  let explanation = "List literal pattern. Only expressions that are lists with %i-elements where each element matches the corresponding element pattern match this *list literal pattern*.";
  {
    id: ListLitPat,
    syntactic_form: [
      mk_list_pat([[pat("p1"), comma_pat(), space(), pat("...")]]),
    ],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let listnil_pat: form = {
  let explanation = "Empty list pattern. Only expressions that are empty lists `[]` match the *empty list `[]` pattern*.";
  {
    id: ListNilPat,
    syntactic_form: [pat("[]")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let _pat_hd = pat("p_hd");
let _pat_tl = pat("p_tl");
let cons_base_pat_coloring_ids =
    (~hd_id: Id.t, ~tl_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat_hd), hd_id),
  (Piece.id(_pat_tl), tl_id),
];
let cons_base_pat: form = {
  let explanation = "Non-empty list pattern. Only expressions that are non-empty lists with *head element* matching the [*head element pattern*](%i) and *tail* list matching the [*tail pattern*](%i) match this non-empty list pattern.";
  {
    id: ConsPat,
    syntactic_form: [_pat_hd, cons_pat(), _pat_tl],
    expandable_id: Some((Piece.id(_pat_tl), [pat("p_tl")])),
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
  let explanation = "Non-empty list pattern. Only expressions that are non-empty lists with *first element* matching the [*first element pattern*](%i), *second element* matching the [*second element pattern*](%i), and *tail* list matching the [*tail pattern*](%i) match this non-empty list pattern.";
  let c = cons_pat();
  {
    id: Cons2Pat,
    syntactic_form: [_pat_fst, cons_pat(), _pat_snd, c, _pat_tl],
    expandable_id:
      Some((Piece.id(c), [pat("p_snd"), cons_pat(), pat("p_tl")])),
    explanation,
    examples: [],
  };
};

let listlit: group = {id: ListLitPat, forms: [listlit_pat]};

let listnil: group = {id: ListNilPat, forms: [listnil_pat]};

let cons: group = {id: ConsPat, forms: [cons_base_pat]};

let cons2: group = {id: Cons2Pat, forms: [cons2_pat, cons_base_pat]};
