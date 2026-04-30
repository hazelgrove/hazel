open Haz3lcore;
open Example;
open ExplainThisForm;

/* `T(X_1, …, X_n)` applies a parameterized type constructor at the
   type level. Three forms by arity of the argument list. */

let _callee = typ("T");

let _general_list = typ("X_1, …, X_n");

let _single_t = typ("X");

let _pair_t1 = typ("X_1");
let _pair_t2 = typ("X_2");

let _triple_t1 = typ("X_1");
let _triple_t2 = typ("X_2");
let _triple_t3 = typ("X_3");

let typ_param_ap_general_coloring_ids =
    (~callee_id: Id.t, ~args_list_id: Id.t, ~extra_ids: list(Id.t))
    : list((Id.t, Id.t)) =>
  [(Piece.id(_callee), callee_id), (Piece.id(_general_list), args_list_id)]
  @ List.map(tid => (Piece.id(_general_list), tid), extra_ids);

let typ_param_ap_single_coloring_ids =
    (~callee_id: Id.t, ~t_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_callee), callee_id),
  (Piece.id(_single_t), t_id),
];

let typ_param_ap_pair_coloring_ids =
    (~callee_id: Id.t, ~t1_id: Id.t, ~t2_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_callee), callee_id),
  (Piece.id(_pair_t1), t1_id),
  (Piece.id(_pair_t2), t2_id),
];

let typ_param_ap_triple_coloring_ids =
    (~callee_id: Id.t, ~t1_id: Id.t, ~t2_id: Id.t, ~t3_id: Id.t)
    : list((Id.t, Id.t)) => [
  (Piece.id(_callee), callee_id),
  (Piece.id(_triple_t1), t1_id),
  (Piece.id(_triple_t2), t2_id),
  (Piece.id(_triple_t3), t3_id),
];

let general_explanation =
    (~callee_str: string, ~callee_id: Id.t, ~args_ids: list(Id.t))
    : string => {
  let args_part =
    args_ids
    |> List.mapi((i, tid) =>
         Printf.sprintf("[*X_%d*](%s)", i + 1, Id.to_string(tid))
       )
    |> String.concat(", ");
  Printf.sprintf(
    "Applies the parameterized type [*%s*](%s) to the type arguments %s. Each argument substitutes for the corresponding parameter of `%s`.",
    callee_str,
    Id.to_string(callee_id),
    args_part,
    callee_str,
  );
};

let single_explanation =
    (~callee_str: string, ~callee_id: Id.t, ~t_id: Id.t): string =>
  Printf.sprintf(
    "Applies the parameterized type [*%s*](%s) to the type argument [*X*](%s). `X` substitutes for the single parameter of `%s`.",
    callee_str,
    Id.to_string(callee_id),
    Id.to_string(t_id),
    callee_str,
  );

let pair_explanation =
    (
      ~callee_str: string,
      ~callee_id: Id.t,
      ~t1_id: Id.t,
      ~t2_id: Id.t,
    )
    : string =>
  Printf.sprintf(
    "Applies the parameterized type [*%s*](%s) to two type arguments [*X_1*](%s) and [*X_2*](%s). Both substitute for the corresponding parameters of `%s` in a single step.",
    callee_str,
    Id.to_string(callee_id),
    Id.to_string(t1_id),
    Id.to_string(t2_id),
    callee_str,
  );

let triple_explanation =
    (
      ~callee_str: string,
      ~callee_id: Id.t,
      ~t1_id: Id.t,
      ~t2_id: Id.t,
      ~t3_id: Id.t,
    )
    : string =>
  Printf.sprintf(
    "Applies the parameterized type [*%s*](%s) to three type arguments [*X_1*](%s), [*X_2*](%s), and [*X_3*](%s). All three substitute for the corresponding parameters of `%s` in a single step.",
    callee_str,
    Id.to_string(callee_id),
    Id.to_string(t1_id),
    Id.to_string(t2_id),
    Id.to_string(t3_id),
    callee_str,
  );

/* The dropdown option previews include the callee prefix `T` so the
   user sees the full `T(…)` form when switching between arities,
   rather than just the parenthesized argument list. Each preview
   uses fresh pieces that mirror the main syntactic_form. */

let _general_ap = mk_parens_typ([[_general_list]]);
let typ_param_ap_general_form: form = {
  let explanation = "Applies a parameterized type to arbitrarily many type arguments at once.";
  let preview = [typ("T"), mk_parens_typ([[typ("X_1, …, X_n")]])];
  {
    id: TypParamApTyp(General),
    syntactic_form: [_callee, _general_ap],
    expandable_id: Some((Piece.id(_general_ap), preview)),
    explanation,
    examples: [],
  };
};

let _single_ap = mk_parens_typ([[_single_t]]);
let typ_param_ap_single_form: form = {
  let explanation = "Applies a parameterized type to a single type argument.";
  let preview = [typ("T"), mk_parens_typ([[typ("X")]])];
  {
    id: TypParamApTyp(Arity1),
    syntactic_form: [_callee, _single_ap],
    expandable_id: Some((Piece.id(_single_ap), preview)),
    explanation,
    examples: [],
  };
};

let _pair_ap =
  mk_parens_typ([[_pair_t1, Example.comma_typ(), space(), _pair_t2]]);
let typ_param_ap_pair_form: form = {
  let explanation = "Applies the parameterized type [*%s*](%s) to two type arguments [*%s*](%s) and [*%s*](%s). Both substitute for the corresponding parameters of `%s` in a single step.";
  let preview = [
    typ("T"),
    mk_parens_typ([
      [typ("X_1"), Example.comma_typ(), space(), typ("X_2")],
    ]),
  ];
  {
    id: TypParamApTyp(Arity2),
    syntactic_form: [_callee, _pair_ap],
    expandable_id: Some((Piece.id(_pair_ap), preview)),
    explanation,
    examples: [],
  };
};

let _triple_ap =
  mk_parens_typ([
    [
      _triple_t1,
      Example.comma_typ(),
      space(),
      _triple_t2,
      Example.comma_typ(),
      space(),
      _triple_t3,
    ],
  ]);
let typ_param_ap_triple_form: form = {
  let explanation = "Applies the parameterized type [*%s*](%s) to three type arguments [*%s*](%s), [*%s*](%s), and [*%s*](%s). All three substitute for the corresponding parameters of `%s` in a single step.";
  let preview = [
    typ("T"),
    mk_parens_typ([
      [
        typ("X_1"),
        Example.comma_typ(),
        space(),
        typ("X_2"),
        Example.comma_typ(),
        space(),
        typ("X_3"),
      ],
    ]),
  ];
  {
    id: TypParamApTyp(Arity3),
    syntactic_form: [_callee, _triple_ap],
    expandable_id: Some((Piece.id(_triple_ap), preview)),
    explanation,
    examples: [],
  };
};

let typ_param_aps_general: group = {
  id: TypParamApTyp(General),
  forms: [typ_param_ap_general_form],
};

let typ_param_aps_single: group = {
  id: TypParamApTyp(Arity1),
  forms: [typ_param_ap_single_form, typ_param_ap_general_form],
};

let typ_param_aps_pair: group = {
  id: TypParamApTyp(Arity2),
  forms: [typ_param_ap_pair_form, typ_param_ap_general_form],
};

let typ_param_aps_triple: group = {
  id: TypParamApTyp(Arity3),
  forms: [typ_param_ap_triple_form, typ_param_ap_general_form],
};
