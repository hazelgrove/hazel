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

/* A parameterized type alias `type T(a, b) = def in body` binds `T`
   as a parameterized type constructor. The parameters `a`, `b`
   appear in `def` as bound type variables; at use sites `T(X, Y)`
   applies the constructor, substituting `X` for `a` and `Y` for `b`
   in `def`. */

let _head_tpat = tpat("T");
let _param_a = tpat("a");
let _param_b = tpat("b");
let _typ_def_p = typ("ty_def");

let param_tyalias_exp_coloring_ids =
    (
      ~head_id: Id.t,
      ~params_ids: list(Id.t),
      ~def_id: Id.t,
    ) => {
  let exemplars = [_param_a, _param_b];
  let head = (Piece.id(_head_tpat), head_id);
  let params =
    List.mapi(
      (i, pid) =>
        if (i < List.length(exemplars)) {
          (Piece.id(List.nth(exemplars, i)), pid);
        } else {
          (pid, pid);
        },
      params_ids,
    );
  [head, (Piece.id(_typ_def_p), def_id), ...params];
};

let param_tyalias_exp: form = {
  let explanation = "Binds [*%s*](%s) as a parameterized type constructor with parameters [*%s*](%s), [*%s*](%s), … Inside the [*definition*](%s) the parameters are abstract type variables; at use sites `%s(X, Y, …)` substitutes each argument for the corresponding parameter.";
  let form = [
    mk_tyalias([
      [
        space(),
        _head_tpat,
        mk_ap_tpat([[_param_a, comma_tpat(), space(), _param_b]]),
        space(),
      ],
      [space(), _typ_def_p, space()],
    ]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: ParameterizedTyAliasExp,
    syntactic_form: form,
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let param_tyalias_exps: group = {
  id: ParameterizedTyAliasExp,
  forms: [param_tyalias_exp],
};
