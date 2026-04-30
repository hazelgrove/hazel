open Haz3lcore;
open ExplainThisForm;
open Example;

/* `e@<t>` / `e@<t, u>` / `e@<t, u, v>` — value-level type
   application. Three forms by arity of the argument list. */

let typfunapp_exp_ex = {
  sub_id: TypFunAp,
  term:
    mk_example(
      "let id : \n poly a -> (a -> a) = \n typfun a -> \n fun x : a -> x \n in id@<Int>",
    ),
  message: "The polymorphic identity function is instantiated at Int. The type variable a is bound to Int in the type function body and the body evaluates to the identity function on integers.",
};

let _f = exp("e_tfun");

let _general_list = typ("X_1, …, X_n");

let _pair_t1 = typ("X_1");
let _pair_t2 = typ("X_2");

let _triple_t1 = typ("X_1");
let _triple_t2 = typ("X_2");
let _triple_t3 = typ("X_3");

let typ_ap_general_coloring_ids =
    (~f_id: Id.t, ~args_list_id: Id.t, ~extra_ids: list(Id.t))
    : list((Id.t, Id.t)) =>
  [(Piece.id(_f), f_id), (Piece.id(_general_list), args_list_id)]
  @ List.map(tid => (Piece.id(_general_list), tid), extra_ids);

let typ_ap_pair_coloring_ids =
    (~f_id: Id.t, ~t1_id: Id.t, ~t2_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_f), f_id),
  (Piece.id(_pair_t1), t1_id),
  (Piece.id(_pair_t2), t2_id),
];

let typ_ap_triple_coloring_ids =
    (~f_id: Id.t, ~t1_id: Id.t, ~t2_id: Id.t, ~t3_id: Id.t)
    : list((Id.t, Id.t)) => [
  (Piece.id(_f), f_id),
  (Piece.id(_triple_t1), t1_id),
  (Piece.id(_triple_t2), t2_id),
  (Piece.id(_triple_t3), t3_id),
];

let general_explanation = (~f_id: Id.t, ~args_ids: list(Id.t)): string => {
  let args_part =
    args_ids
    |> List.mapi((i, tid) =>
         Printf.sprintf("[*X_%d*](%s)", i + 1, Id.to_string(tid))
       )
    |> String.concat(", ");
  Printf.sprintf(
    "Applies the [*type function*](%s) to the type arguments %s. Paired against a matching multi-binder `typfun p_1, …, p_n -> e'`, all binders are substituted in a single step.",
    Id.to_string(f_id),
    args_part,
  );
};

let _general_ap = mk_ap_exp_typ([[_general_list]]);
let typ_ap_general: form = {
  let explanation = "Applies a type function to a list of type arguments at once.";
  {
    id: TypFunApExp(General),
    syntactic_form: [_f, _general_ap],
    expandable_id: Some((Piece.id(_general_ap), [_general_ap])),
    explanation,
    examples: [typfunapp_exp_ex],
  };
};

let _pair_ap =
  mk_ap_exp_typ([[_pair_t1, Example.comma_typ(), space(), _pair_t2]]);
let typ_ap_pair: form = {
  let explanation = "Applies the [*type function*](%s) to two type arguments [*%s*](%s) and [*%s*](%s). Paired against a matching multi-binder `typfun p_1, p_2 -> e'`, both binders are substituted in a single step.";
  {
    id: TypFunApExp(Arity2),
    syntactic_form: [_f, _pair_ap],
    expandable_id: Some((Piece.id(_pair_ap), [_pair_ap])),
    explanation,
    examples: [typfunapp_exp_ex],
  };
};

let _triple_ap =
  mk_ap_exp_typ([
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
let typ_ap_triple: form = {
  let explanation = "Applies the [*type function*](%s) to three type arguments [*%s*](%s), [*%s*](%s), and [*%s*](%s). Paired against a matching multi-binder `typfun p_1, p_2, p_3 -> e'`, all three binders are substituted in a single step.";
  {
    id: TypFunApExp(Arity3),
    syntactic_form: [_f, _triple_ap],
    expandable_id: Some((Piece.id(_triple_ap), [_triple_ap])),
    explanation,
    examples: [typfunapp_exp_ex],
  };
};

let typ_aps_general: group = {
  id: TypFunApExp(General),
  forms: [typ_ap_general],
};

let typ_aps_pair: group = {
  id: TypFunApExp(Arity2),
  forms: [typ_ap_pair, typ_ap_general],
};

let typ_aps_triple: group = {
  id: TypFunApExp(Arity3),
  forms: [typ_ap_triple, typ_ap_general],
};
