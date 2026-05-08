open Haz3lcore;
open ExplainThisForm;
open Example;

/* An `abs` value-level type abstraction (System F's big-Lambda).
   Three forms by arity of the binder list, mirroring PolyTyp.
   Renamed from the surface keyword `typfun` so `typfun` is now the
   *type-level* type function (`type T = typfun a -> body`). */

let poly_id_ex = {
  sub_id: TypAbs(Basic),
  term:
    mk_example(
      "let id : \n poly a -> (a -> a) = \n abs a -> \n fun x : a -> x \n in id",
    ),
  message: "The polymorphic identity function. It may be instantiated at any type a, after which the function acts as type (a -> a).",
};

let _body = exp("e");

let _general_list = tpat("p_1, …, p_n");

let _single_p = tpat("p");

let _pair_p1 = tpat("p_1");
let _pair_p2 = tpat("p_2");

let _triple_p1 = tpat("p_1");
let _triple_p2 = tpat("p_2");
let _triple_p3 = tpat("p_3");

let typabs_general_coloring_ids =
    (~binders_list_id: Id.t, ~extra_ids: list(Id.t), ~body_id: Id.t)
    : list((Id.t, Id.t)) =>
  [(Piece.id(_general_list), binders_list_id), (Piece.id(_body), body_id)]
  @ List.map(pid => (Piece.id(_general_list), pid), extra_ids);

let typabs_single_coloring_ids =
    (~p_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_single_p), p_id),
  (Piece.id(_body), body_id),
];

let typabs_pair_coloring_ids =
    (~p1_id: Id.t, ~p2_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pair_p1), p1_id),
  (Piece.id(_pair_p2), p2_id),
  (Piece.id(_body), body_id),
];

let typabs_triple_coloring_ids =
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
    "A value-level type abstraction over type variables %s. A type application `@<X_1, …, X_n>` substitutes each argument for the corresponding variable in [*the body*](%s).",
    binders_part,
    Id.to_string(body_id),
  );
};

let single_explanation = (~p_id: Id.t, ~body_id: Id.t): string =>
  Printf.sprintf(
    "A value-level type abstraction over a type variable [*p*](%s). A type application `@<X>` substitutes `X` for `p` in [*the body*](%s).",
    Id.to_string(p_id),
    Id.to_string(body_id),
  );

let pair_explanation = (~p1_id: Id.t, ~p2_id: Id.t, ~body_id: Id.t): string =>
  Printf.sprintf(
    "A value-level type abstraction over two type variables [*p_1*](%s) and [*p_2*](%s). A type application `@<X_1, X_2>` substitutes both in [*the body*](%s) in a single step.",
    Id.to_string(p1_id),
    Id.to_string(p2_id),
    Id.to_string(body_id),
  );

let triple_explanation =
    (~p1_id: Id.t, ~p2_id: Id.t, ~p3_id: Id.t, ~body_id: Id.t): string =>
  Printf.sprintf(
    "A value-level type abstraction over three type variables [*p_1*](%s), [*p_2*](%s), and [*p_3*](%s). A type application `@<X_1, X_2, X_3>` substitutes all three in [*the body*](%s) in a single step.",
    Id.to_string(p1_id),
    Id.to_string(p2_id),
    Id.to_string(p3_id),
    Id.to_string(body_id),
  );

let _general_head = mk_typabs([[space(), _general_list, space()]]);
let typabs_general: form = {
  let explanation = "A value-level type abstraction over arbitrarily many type variables.";
  {
    id: TypAbsExp(General),
    syntactic_form: [_general_head, space(), _body],
    expandable_id: Some((Piece.id(_general_head), [_general_head])),
    explanation,
    examples: [poly_id_ex],
  };
};

let _single_head = mk_typabs([[space(), _single_p, space()]]);
let typabs_single: form = {
  let explanation = "A value-level type abstraction over a single type variable.";
  {
    id: TypAbsExp(Arity1),
    syntactic_form: [_single_head, space(), _body],
    expandable_id: Some((Piece.id(_single_head), [_single_head])),
    explanation,
    examples: [poly_id_ex],
  };
};

let _pair_head =
  mk_typabs([[space(), _pair_p1, comma_tpat(), space(), _pair_p2, space()]]);
let typabs_pair: form = {
  let explanation = "A value-level type abstraction over two type variables [*%s*](%s) and [*%s*](%s). A type application `@<X_1, X_2>` substitutes both in [*the body*](%s) in a single step.";
  {
    id: TypAbsExp(Arity2),
    syntactic_form: [_pair_head, space(), _body],
    expandable_id: Some((Piece.id(_pair_head), [_pair_head])),
    explanation,
    examples: [poly_id_ex],
  };
};

let _triple_head =
  mk_typabs([
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
let typabs_triple: form = {
  let explanation = "A value-level type abstraction over three type variables [*%s*](%s), [*%s*](%s), and [*%s*](%s). A type application `@<X_1, X_2, X_3>` substitutes all three in [*the body*](%s) in a single step.";
  {
    id: TypAbsExp(Arity3),
    syntactic_form: [_triple_head, space(), _body],
    expandable_id: Some((Piece.id(_triple_head), [_triple_head])),
    explanation,
    examples: [poly_id_ex],
  };
};

let type_abstractions_general: group = {
  id: TypAbsExp(General),
  forms: [typabs_general],
};

let type_abstractions_single: group = {
  id: TypAbsExp(Arity1),
  forms: [typabs_single, typabs_general],
};

let type_abstractions_pair: group = {
  id: TypAbsExp(Arity2),
  forms: [typabs_pair, typabs_general],
};

let type_abstractions_triple: group = {
  id: TypAbsExp(Arity3),
  forms: [typabs_triple, typabs_general],
};
