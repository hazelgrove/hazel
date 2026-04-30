open Haz3lcore;
open Example;
open ExplainThisForm;

/* A `poly` universal type. Three forms by arity of the binder list:
   - General: `poly p_1, …, p_n -> body` covers arbitrary arity.
   - Arity2: `poly p_1, p_2 -> body`.
   - Arity3: `poly p_1, p_2, p_3 -> body`. */

let _body = typ("ty_body");

let _general_list = tpat("p_1, …, p_n");

let _single_p = tpat("p");

let _pair_p1 = tpat("p_1");
let _pair_p2 = tpat("p_2");

let _triple_p1 = tpat("p_1");
let _triple_p2 = tpat("p_2");
let _triple_p3 = tpat("p_3");

let poly_typ_general_coloring_ids =
    (~binders_list_id: Id.t, ~extra_ids: list(Id.t), ~body_id: Id.t)
    : list((Id.t, Id.t)) =>
  [
    (Piece.id(_general_list), binders_list_id),
    (Piece.id(_body), body_id),
  ]
  @ List.map(pid => (Piece.id(_general_list), pid), extra_ids);

let poly_typ_single_coloring_ids =
    (~p_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_single_p), p_id),
  (Piece.id(_body), body_id),
];

let poly_typ_pair_coloring_ids =
    (~p1_id: Id.t, ~p2_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pair_p1), p1_id),
  (Piece.id(_pair_p2), p2_id),
  (Piece.id(_body), body_id),
];

let poly_typ_triple_coloring_ids =
    (~p1_id: Id.t, ~p2_id: Id.t, ~p3_id: Id.t, ~body_id: Id.t)
    : list((Id.t, Id.t)) => [
  (Piece.id(_triple_p1), p1_id),
  (Piece.id(_triple_p2), p2_id),
  (Piece.id(_triple_p3), p3_id),
  (Piece.id(_body), body_id),
];

let general_explanation = (~binders_ids: list(Id.t), ~body_id: Id.t): string => {
  let binders_part =
    binders_ids
    |> List.mapi((i, pid) =>
         Printf.sprintf("[*p_%d*](%s)", i + 1, Id.to_string(pid))
       )
    |> String.concat(", ");
  Printf.sprintf(
    "A universal type quantifying over type variables %s. Classifies polymorphic values; a type application `@<X_1, …, X_n>` substitutes each argument for the corresponding variable in [*the body*](%s).",
    binders_part,
    Id.to_string(body_id),
  );
};

let single_explanation = (~p_id: Id.t, ~body_id: Id.t): string =>
  Printf.sprintf(
    "A universal type quantifying over a type variable [*p*](%s). Classifies polymorphic values; a type application `@<X>` substitutes `X` for `p` in [*the body*](%s).",
    Id.to_string(p_id),
    Id.to_string(body_id),
  );

let pair_explanation = (~p1_id: Id.t, ~p2_id: Id.t, ~body_id: Id.t): string =>
  Printf.sprintf(
    "A universal type quantifying over two type variables [*p_1*](%s) and [*p_2*](%s). A type application `@<X_1, X_2>` substitutes both in [*the body*](%s) in a single step.",
    Id.to_string(p1_id),
    Id.to_string(p2_id),
    Id.to_string(body_id),
  );

let triple_explanation =
    (~p1_id: Id.t, ~p2_id: Id.t, ~p3_id: Id.t, ~body_id: Id.t): string =>
  Printf.sprintf(
    "A universal type quantifying over three type variables [*p_1*](%s), [*p_2*](%s), and [*p_3*](%s). A type application `@<X_1, X_2, X_3>` substitutes all three in [*the body*](%s) in a single step.",
    Id.to_string(p1_id),
    Id.to_string(p2_id),
    Id.to_string(p3_id),
    Id.to_string(body_id),
  );

let _general_head = mk_poly([[space(), _general_list, space()]]);
let poly_typ_general: form = {
  /* The explanation is supplied dynamically via `~explanation` at
     dispatch time so every user binder gets its own `[*p_i*](id)`
     link and highlight. This placeholder only appears in isolation. */
  let explanation = "A universal type quantifying over arbitrarily many type variables.";
  {
    id: PolyTyp(General),
    syntactic_form: [_general_head, _body],
    expandable_id: Some((Piece.id(_general_head), [_general_head])),
    explanation,
    examples: [],
  };
};

let _single_head = mk_poly([[space(), _single_p, space()]]);
let poly_typ_single: form = {
  let explanation = "A universal type quantifying over a single type variable.";
  {
    id: PolyTyp(Arity1),
    syntactic_form: [_single_head, _body],
    expandable_id: Some((Piece.id(_single_head), [_single_head])),
    explanation,
    examples: [],
  };
};

let _pair_head =
  mk_poly([[space(), _pair_p1, comma_tpat(), space(), _pair_p2, space()]]);
let poly_typ_pair: form = {
  let explanation = "A universal type quantifying over two type variables [*%s*](%s) and [*%s*](%s). A type application `@<X_1, X_2>` substitutes both in [*the body*](%s) in a single step.";
  {
    id: PolyTyp(Arity2),
    syntactic_form: [_pair_head, _body],
    expandable_id: Some((Piece.id(_pair_head), [_pair_head])),
    explanation,
    examples: [],
  };
};

let _triple_head =
  mk_poly([
    [
      space(),
      _triple_p1,
      comma_tpat(),
      space(),
      _triple_p2,
      comma_tpat(),
      space(),
      _triple_p3,
      space(),
    ],
  ]);
let poly_typ_triple: form = {
  let explanation = "A universal type quantifying over three type variables [*%s*](%s), [*%s*](%s), and [*%s*](%s). A type application `@<X_1, X_2, X_3>` substitutes all three in [*the body*](%s) in a single step.";
  {
    id: PolyTyp(Arity3),
    syntactic_form: [_triple_head, _body],
    expandable_id: Some((Piece.id(_triple_head), [_triple_head])),
    explanation,
    examples: [],
  };
};

let poly_typ_general_group: group = {
  id: PolyTyp(General),
  forms: [poly_typ_general],
};

let poly_typ_single_group: group = {
  id: PolyTyp(Arity1),
  forms: [poly_typ_single, poly_typ_general],
};

let poly_typ_pair_group: group = {
  id: PolyTyp(Arity2),
  forms: [poly_typ_pair, poly_typ_general],
};

let poly_typ_triple_group: group = {
  id: PolyTyp(Arity3),
  forms: [poly_typ_triple, poly_typ_general],
};
