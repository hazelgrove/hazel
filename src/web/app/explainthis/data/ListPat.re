open Haz3lcore;
open Example;
open ExplainThisForm;

let listlit_pat_form = [
  mk_list_pat([[pat("p1"), comma_pat(), space(), pat("...")]]),
];
let listlit_pat = (~n: int): form => {
  id: ListLitPat,
  syntactic_form: listlit_pat_form,
  colorings: [],
  expandable_id: None,
  explanation:
    Printf.sprintf(
      "Only expressions that are lists with %d-elements where each element matches the corresponding element pattern match this *list literal pattern*.",
      n,
    ),
  examples: [],
};
let listnil_pat: form = {
  let explanation = "Only expressions that are empty lists `[]` match the *empty list `[]` pattern*.";
  {
    id: ListNilPat,
    syntactic_form: [pat("[]")],
    colorings: [],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let pat_hd = pat("p_hd");
let pat_tl = pat("p_tl");
let cons_base_pat_coloring_ids =
    (~hd_id: Id.t, ~tl_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(pat_hd), hd_id),
  (Piece.id(pat_tl), tl_id),
];
let cons_base_pat_id: form_id = ConsPat;
let cons_base_pat_form = [pat_hd, cons_pat(), pat_tl];
let cons_base_pat_explanation = (~hd_id: Id.t, ~tl_id: Id.t): string =>
  Printf.sprintf(
    "Only expressions that are non-empty lists with *head element* matching the [*head element pattern*](%s) and *tail* list matching the [*tail pattern*](%s) match this non-empty list pattern.",
    Id.to_string(hd_id),
    Id.to_string(tl_id),
  );
let cons_base_pat = (~hd_id: Id.t, ~tl_id: Id.t): form => {
  id: cons_base_pat_id,
  syntactic_form: cons_base_pat_form,
  colorings: cons_base_pat_coloring_ids(~hd_id, ~tl_id),
  expandable_id: Some((Piece.id(pat_tl), [pat("p_tl")])),
  explanation: cons_base_pat_explanation(~hd_id, ~tl_id),
  examples: [],
};
let pat_fst = pat("p_fst");
let pat_snd = pat("p_snd");
let pat_tl = pat("p_tl");
let cons2_pat_coloring_ids =
    (~fst_id: Id.t, ~snd_id: Id.t, ~tl_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(pat_fst), fst_id),
  (Piece.id(pat_snd), snd_id),
  (Piece.id(pat_tl), tl_id),
];
let cons2_pat_id: form_id = Cons2Pat;
let cons2_pat_c = cons_pat();
let cons2_pat_form = [pat_fst, cons_pat(), pat_snd, cons2_pat_c, pat_tl];
let cons2_pat = (~fst_id: Id.t, ~snd_id: Id.t, ~tl_id: Id.t): form => {
  id: cons2_pat_id,
  syntactic_form: cons2_pat_form,
  colorings: cons2_pat_coloring_ids(~fst_id, ~snd_id, ~tl_id),
  expandable_id:
    Some((
      Piece.id(cons2_pat_c),
      [pat("p_snd"), cons_pat(), pat("p_tl")],
    )),
  explanation:
    Printf.sprintf(
      "Only expressions that are non-empty lists with *first element* matching the [*first element pattern*](%s), *second element* matching the [*second element pattern*](%s), and *tail* list matching the [*tail pattern*](%s) match this non-empty list pattern.",
      Id.to_string(fst_id),
      Id.to_string(snd_id),
      Id.to_string(tl_id),
    ),
  examples: [],
};

let listlit = (~n: int): group => {
  id: ListLitPat,
  forms: [listlit_pat(~n)],
};

let listnil: group = {
  id: ListNilPat,
  forms: [listnil_pat],
};

let cons = (~hd_id: Id.t, ~tl_id: Id.t): group => {
  id: ConsPat,
  forms: [cons_base_pat(~hd_id, ~tl_id)],
};

let cons2 = (~fst_id: Id.t, ~snd_id: Id.t, ~tl_id: Id.t, ~hd_id: Id.t): group => {
  id: Cons2Pat,
  forms: [
    cons2_pat(~fst_id, ~snd_id, ~tl_id),
    cons_base_pat(~hd_id, ~tl_id),
  ],
};
