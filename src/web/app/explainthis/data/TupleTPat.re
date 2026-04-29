open Haz3lcore;
open Example;
open ExplainThisForm;

/* `a, b, …` comma-separated type binders as the binder of
   `poly`/`typfun`/`typlam`/`rec`. Three forms by arity. */

let _general_list = tpat("p_1, …, p_n");

let _pair_p1 = tpat("p_1");
let _pair_p2 = tpat("p_2");

let _triple_p1 = tpat("p_1");
let _triple_p2 = tpat("p_2");
let _triple_p3 = tpat("p_3");

let tuple_tpat_general_coloring_ids =
    (~binders_list_id: Id.t, ~extra_ids: list(Id.t))
    : list((Id.t, Id.t)) =>
  [(Piece.id(_general_list), binders_list_id)]
  @ List.map(pid => (Piece.id(_general_list), pid), extra_ids);

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

let general_explanation = (~binders_ids: list(Id.t)): string => {
  let binders_part =
    binders_ids
    |> List.mapi((i, pid) =>
         Printf.sprintf(
           "[*p_%d*](%s)",
           i + 1,
           Id.to_string(pid),
         )
       )
    |> String.concat(", ");
  Printf.sprintf(
    "A comma-separated list of type binders %s used as the binder of a multi-binder `poly` or `typfun`. Each binder introduces a fresh type variable in scope throughout the body.",
    binders_part,
  );
};

let tuple_tpat_general_form: form = {
  let explanation = "A comma-separated list of type binders used as the binder of a multi-binder `poly` or `typfun`.";
  {
    id: TupleTPat(General),
    syntactic_form: [_general_list],
    expandable_id: Some((Piece.id(_general_list), [_general_list])),
    explanation,
    examples: [],
  };
};

let tuple_tpat_pair_form: form = {
  let explanation = "A comma-separated list of two type binders [*%s*](%s), [*%s*](%s). Both are fresh type variables in scope throughout the body.";
  let comma1 = comma_tpat();
  {
    id: TupleTPat(Arity2),
    syntactic_form: [_pair_p1, comma1, space(), _pair_p2],
    expandable_id: Some((Piece.id(comma1), [_pair_p1, comma_tpat(), space(), _pair_p2])),
    explanation,
    examples: [],
  };
};

let tuple_tpat_triple_form: form = {
  let explanation = "A comma-separated list of three type binders [*%s*](%s), [*%s*](%s), [*%s*](%s). All three are fresh type variables in scope throughout the body.";
  let comma1 = comma_tpat();
  {
    id: TupleTPat(Arity3),
    syntactic_form: [
      _triple_p1,
      comma1,
      space(),
      _triple_p2,
      comma_tpat(),
      space(),
      _triple_p3,
    ],
    expandable_id:
      Some((
        Piece.id(comma1),
        [
          _triple_p1,
          comma_tpat(),
          space(),
          _triple_p2,
          comma_tpat(),
          space(),
          _triple_p3,
        ],
      )),
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
