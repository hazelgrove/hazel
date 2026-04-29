open Haz3lcore;
open Example;
open ExplainThisForm;

/* `T(a, b, …)` as a type pattern at the head of a parameterized
   type alias declaration. Three forms by arity — the 2- and
   3-parameter forms highlight each parameter individually; the
   general form covers arbitrary arity. */

let _head = tpat("T");
let _general_ps = tpat("p_1, …, p_n");

let _pair_p1 = tpat("p_1");
let _pair_p2 = tpat("p_2");

let _triple_p1 = tpat("p_1");
let _triple_p2 = tpat("p_2");
let _triple_p3 = tpat("p_3");

let param_tpat_general_coloring_ids =
    (
      ~head_id: Id.t,
      ~params_list_id: Id.t,
      ~extra_ids: list(Id.t),
    )
    : list((Id.t, Id.t)) =>
  [(Piece.id(_head), head_id), (Piece.id(_general_ps), params_list_id)]
  @ List.map(pid => (Piece.id(_general_ps), pid), extra_ids);

let param_tpat_pair_coloring_ids =
    (~head_id: Id.t, ~p1_id: Id.t, ~p2_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_head), head_id),
  (Piece.id(_pair_p1), p1_id),
  (Piece.id(_pair_p2), p2_id),
];

let param_tpat_triple_coloring_ids =
    (
      ~head_id: Id.t,
      ~p1_id: Id.t,
      ~p2_id: Id.t,
      ~p3_id: Id.t,
    )
    : list((Id.t, Id.t)) => [
  (Piece.id(_head), head_id),
  (Piece.id(_triple_p1), p1_id),
  (Piece.id(_triple_p2), p2_id),
  (Piece.id(_triple_p3), p3_id),
];

/* Build the explanation string for the general-arity case.
   Produces one `[*p_i*](id)` link per parameter, so each user-side
   parameter token gets its own highlight color even at arity 4+. */
let general_explanation = (~head_str: string, ~params_ids: list(Id.t)): string => {
  let params_part =
    params_ids
    |> List.mapi((i, pid) =>
         Printf.sprintf(
           "[*p_%d*](%s)",
           i + 1,
           Id.to_string(pid),
         )
       )
    |> String.concat(", ");
  Printf.sprintf(
    "`%s` is a parameterized type constructor bound in the body. Its parameters %s are type variables available inside the definition.",
    head_str,
    params_part,
  );
};

let _general_ap = mk_ap_tpat([[_general_ps]]);
let param_tpat_general_form: form = {
  /* The explanation is supplied dynamically via `~explanation` at
     dispatch time; this placeholder string is only shown if the
     form is viewed in isolation. */
  let explanation = "A parameterized type constructor with arbitrarily many parameters.";
  {
    id: ParamTPat(General),
    syntactic_form: [_head, _general_ap],
    expandable_id: Some((Piece.id(_general_ap), [_general_ap])),
    explanation,
    examples: [],
  };
};

let _pair_ap =
  mk_ap_tpat([[_pair_p1, comma_tpat(), space(), _pair_p2]]);
let param_tpat_pair_form: form = {
  let explanation = "`%s` is a parameterized type constructor with two parameters [*%s*](%s) and [*%s*](%s). Both are type variables bound in the definition.";
  {
    id: ParamTPat(Arity2),
    syntactic_form: [_head, _pair_ap],
    expandable_id: Some((Piece.id(_pair_ap), [_pair_ap])),
    explanation,
    examples: [],
  };
};

let _triple_ap =
  mk_ap_tpat([
    [
      _triple_p1,
      comma_tpat(),
      space(),
      _triple_p2,
      comma_tpat(),
      space(),
      _triple_p3,
    ],
  ]);
let param_tpat_triple_form: form = {
  let explanation = "`%s` is a parameterized type constructor with three parameters [*%s*](%s), [*%s*](%s), and [*%s*](%s). All three are type variables bound in the definition.";
  {
    id: ParamTPat(Arity3),
    syntactic_form: [_head, _triple_ap],
    expandable_id: Some((Piece.id(_triple_ap), [_triple_ap])),
    explanation,
    examples: [],
  };
};

let param_tpats_general: group = {
  id: ParamTPat(General),
  forms: [param_tpat_general_form],
};

let param_tpats_arity2: group = {
  id: ParamTPat(Arity2),
  forms: [param_tpat_pair_form, param_tpat_general_form],
};

let param_tpats_arity3: group = {
  id: ParamTPat(Arity3),
  forms: [param_tpat_triple_form, param_tpat_general_form],
};
