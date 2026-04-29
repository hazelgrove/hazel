open Haz3lcore;
open Example;
open ExplainThisForm;

/* `a, b, …` comma-separated type binders as the binder of
   `poly`/`typfun`/`typlam`/`rec`. Three forms by arity — specific
   2- and 3-binder forms highlight each binder, general covers
   arbitrary arity. */

let _general_list = tpat("p_1, …, p_n");

let _pair_p1 = tpat("p_1");
let _pair_p2 = tpat("p_2");

let _triple_p1 = tpat("p_1");
let _triple_p2 = tpat("p_2");
let _triple_p3 = tpat("p_3");

let tuple_tpat_general_coloring_ids =
    (~binders_list_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_general_list), binders_list_id),
];

let tuple_tpat_pair_coloring_ids =
    (~p1_id: Id.t, ~p2_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pair_p1), p1_id),
  (Piece.id(_pair_p2), p2_id),
];

let tuple_tpat_triple_coloring_ids =
    (~p1_id: Id.t, ~p2_id: Id.t, ~p3_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_triple_p1), p1_id),
  (Piece.id(_triple_p2), p2_id),
  (Piece.id(_triple_p3), p3_id),
];

let tuple_tpat_general_form: form = {
  let explanation = "A comma-separated list of [*type binders*](%s) used as the binder of a multi-binder `poly` or `typfun`. Each binder introduces a fresh type variable in scope throughout the body.";
  {
    id: TupleTPat(General),
    syntactic_form: [_general_list],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let tuple_tpat_pair_form: form = {
  let explanation = "A comma-separated list of two type binders [*%s*](%s), [*%s*](%s). Both are fresh type variables in scope throughout the body.";
  {
    id: TupleTPat(Arity2),
    syntactic_form: [_pair_p1, comma_tpat(), space(), _pair_p2],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let tuple_tpat_triple_form: form = {
  let explanation = "A comma-separated list of three type binders [*%s*](%s), [*%s*](%s), [*%s*](%s). All three are fresh type variables in scope throughout the body.";
  {
    id: TupleTPat(Arity3),
    syntactic_form: [
      _triple_p1,
      comma_tpat(),
      space(),
      _triple_p2,
      comma_tpat(),
      space(),
      _triple_p3,
    ],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let tuple_tpats_general: group = {
  id: TupleTPat(General),
  forms: [tuple_tpat_general_form],
};

let tuple_tpats_arity2: group = {
  id: TupleTPat(Arity2),
  forms: [tuple_tpat_pair_form, tuple_tpat_general_form],
};

let tuple_tpats_arity3: group = {
  id: TupleTPat(Arity3),
  forms: [tuple_tpat_triple_form, tuple_tpat_general_form],
};
