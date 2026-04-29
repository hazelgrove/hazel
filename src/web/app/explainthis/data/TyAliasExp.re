open Haz3lcore;
open Example;
open ExplainThisForm;

let _tpat = tpat("p");
let _typ_def = typ("ty_def");
let tyalias_base_exp_coloring_ids = (~tpat_id: Id.t, ~def_id: Id.t) => [
  (Piece.id(_tpat), tpat_id),
  (Piece.id(_typ_def), def_id),
];
let tyalias_exp: form = {
  let explanation = "The [*type*](%s) is bound to the [*type variable*](%s) in the body.";
  let form = [
    mk_tyalias([[space(), _tpat, space()], [space(), _typ_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: TyAliasExp,
    syntactic_form: form,
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let tyalias_exps: group = {
  id: TyAliasExp,
  forms: [tyalias_exp],
};

/* A parameterized type alias `type T(a, …) = def in body` binds `T`
   as a parameterized type constructor. Three forms by arity, with
   the 2- and 3-parameter forms highlighting each parameter
   individually and a general form covering arbitrary arity. */

let _head_tpat_g = tpat("T");
let _general_params = tpat("p_1, …, p_n");
let _typ_def_g = typ("ty_def");

let _head_tpat_p = tpat("T");
let _pair_a = tpat("p_1");
let _pair_b = tpat("p_2");
let _typ_def_p = typ("ty_def");

let _head_tpat_t = tpat("T");
let _triple_a = tpat("p_1");
let _triple_b = tpat("p_2");
let _triple_c = tpat("p_3");
let _typ_def_t = typ("ty_def");

let param_tyalias_general_coloring_ids =
    (~head_id: Id.t, ~params_list_id: Id.t, ~def_id: Id.t) => [
  (Piece.id(_head_tpat_g), head_id),
  (Piece.id(_general_params), params_list_id),
  (Piece.id(_typ_def_g), def_id),
];

let param_tyalias_pair_coloring_ids =
    (~head_id: Id.t, ~p1_id: Id.t, ~p2_id: Id.t, ~def_id: Id.t) => [
  (Piece.id(_head_tpat_p), head_id),
  (Piece.id(_pair_a), p1_id),
  (Piece.id(_pair_b), p2_id),
  (Piece.id(_typ_def_p), def_id),
];

let param_tyalias_triple_coloring_ids =
    (
      ~head_id: Id.t,
      ~p1_id: Id.t,
      ~p2_id: Id.t,
      ~p3_id: Id.t,
      ~def_id: Id.t,
    ) => [
  (Piece.id(_head_tpat_t), head_id),
  (Piece.id(_triple_a), p1_id),
  (Piece.id(_triple_b), p2_id),
  (Piece.id(_triple_c), p3_id),
  (Piece.id(_typ_def_t), def_id),
];

let param_tyalias_general_exp: form = {
  let explanation = "Binds [*%s*](%s) as a parameterized type constructor. Its [*parameters*](%s) are abstract type variables inside the [*definition*](%s); at use sites `%s(X_1, …, X_n)` substitutes each argument for the corresponding parameter.";
  let form = [
    mk_tyalias([
      [
        space(),
        _head_tpat_g,
        mk_ap_tpat([[_general_params]]),
        space(),
      ],
      [space(), _typ_def_g, space()],
    ]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: ParameterizedTyAliasExp(General),
    syntactic_form: form,
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let param_tyalias_pair_exp: form = {
  let explanation = "Binds [*%s*](%s) as a parameterized type constructor with two parameters [*%s*](%s) and [*%s*](%s). Inside the [*definition*](%s) the parameters are abstract type variables; at use sites `%s(X_1, X_2)` substitutes each argument for the corresponding parameter.";
  let form = [
    mk_tyalias([
      [
        space(),
        _head_tpat_p,
        mk_ap_tpat([[_pair_a, comma_tpat(), space(), _pair_b]]),
        space(),
      ],
      [space(), _typ_def_p, space()],
    ]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: ParameterizedTyAliasExp(Arity2),
    syntactic_form: form,
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let param_tyalias_triple_exp: form = {
  let explanation = "Binds [*%s*](%s) as a parameterized type constructor with three parameters [*%s*](%s), [*%s*](%s), and [*%s*](%s). Inside the [*definition*](%s) the parameters are abstract type variables; at use sites `%s(X_1, X_2, X_3)` substitutes each argument for the corresponding parameter.";
  let form = [
    mk_tyalias([
      [
        space(),
        _head_tpat_t,
        mk_ap_tpat([
          [
            _triple_a,
            comma_tpat(),
            space(),
            _triple_b,
            comma_tpat(),
            space(),
            _triple_c,
          ],
        ]),
        space(),
      ],
      [space(), _typ_def_t, space()],
    ]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: ParameterizedTyAliasExp(Arity3),
    syntactic_form: form,
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let param_tyalias_exps_general: group = {
  id: ParameterizedTyAliasExp(General),
  forms: [param_tyalias_general_exp],
};

let param_tyalias_exps_arity2: group = {
  id: ParameterizedTyAliasExp(Arity2),
  forms: [param_tyalias_pair_exp, param_tyalias_general_exp],
};

let param_tyalias_exps_arity3: group = {
  id: ParameterizedTyAliasExp(Arity3),
  forms: [param_tyalias_triple_exp, param_tyalias_general_exp],
};
