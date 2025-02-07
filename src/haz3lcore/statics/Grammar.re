open Util;

module Annotated = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('a, 'b) = {
    term: 'a,
    annotation: 'b,
  };

  let term_of = x => x.term;
  let unwrap = x => (x.term, term' => {...x, term: term'});
};

[@deriving (show({with_path: false}), sexp, yojson)]
type deferral_position_t =
  | InAp
  | OutsideAp;

[@deriving (show({with_path: false}), sexp, yojson)]
type any_t('a) =
  | Exp(exp_t('a))
  | Pat(pat_t('a))
  | Typ(typ_t('a))
  | TPat(tpat_t('a))
  | Rul(rul_t('a))
  | Any(unit)
and exp_term('a) =
  | Invalid(string)
  | EmptyHole
  | MultiHole(list(any_t('a)))
  | DynamicErrorHole(exp_t('a), InvalidOperationError.t)
  | FailedCast(exp_t('a), typ_t('a), typ_t('a))
  | Deferral(deferral_position_t)
  | Undefined
  | Bool(bool)
  | Int(int)
  | Float(float)
  | String(string)
  | ListLit(list(exp_t('a)))
  | Constructor(string, typ_t('a)) // Typ.t field is only meaningful in dynamic expressions
  | Fun(pat_t('a), exp_t('a), option(typ_t('a)), option(Var.t)) // typ_t field is only used to display types in results
  | TypFun(tpat_t('a), exp_t('a), option(Var.t))
  | Tuple(list(exp_t('a)))
  | Label(string)
  | TupLabel(exp_t('a), exp_t('a))
  | Dot(exp_t('a), exp_t('a))
  | Var(Var.t)
  | Let(pat_t('a), exp_t('a), exp_t('a))
  | FixF(pat_t('a), exp_t('a), option(closure_environment_t('a)))
  | TyAlias(tpat_t('a), typ_t('a), exp_t('a))
  | Ap(Operators.ap_direction, exp_t('a), exp_t('a))
  | TypAp(exp_t('a), typ_t('a))
  | DeferredAp(exp_t('a), list(exp_t('a)))
  | If(exp_t('a), exp_t('a), exp_t('a))
  | Seq(exp_t('a), exp_t('a))
  | Test(exp_t('a))
  | Filter(stepper_filter_kind_t('a), exp_t('a))
  | Closure([@show.opaque] closure_environment_t('a), exp_t('a))
  | Parens(exp_t('a)) // (
  | Cons(exp_t('a), exp_t('a))
  | ListConcat(exp_t('a), exp_t('a))
  | UnOp(Operators.op_un, exp_t('a))
  | BinOp(Operators.op_bin, exp_t('a), exp_t('a))
  | BuiltinFun(string)
  | Match(exp_t('a), list((pat_t('a), exp_t('a))))
  /* INVARIANT: in dynamic expressions, casts must be between
     two consistent types. Both types should be normalized in
     dynamics for the cast calculus to work right. */
  | Cast(exp_t('a), typ_t('a), typ_t('a))
and exp_t('a) = Annotated.t(exp_term('a), 'a)
and pat_term('a) =
  | Invalid(string)
  | EmptyHole
  | MultiHole(list(any_t('a)))
  | Wild
  | Int(int)
  | Float(float)
  | Bool(bool)
  | String(string)
  | ListLit(list(pat_t('a)))
  | Constructor(string, typ_t('a)) // Typ.t field is only meaningful in dynamic patterns
  | Cons(pat_t('a), pat_t('a))
  | Var(Var.t)
  | Tuple(list(pat_t('a)))
  | Label(string)
  | TupLabel(pat_t('a), pat_t('a))
  | Parens(pat_t('a))
  | Ap(pat_t('a), pat_t('a))
  | Cast(pat_t('a), typ_t('a), typ_t('a))
and pat_t('a) = Annotated.t(pat_term('a), 'a)
and typ_term('a) =
  | Unknown(type_provenance('a))
  | Int
  | Float
  | Bool
  | String
  | Var(string)
  | List(typ_t('a))
  | Arrow(typ_t('a), typ_t('a))
  | Sum(ConstructorMap.t(typ_t('a)))
  | Prod(list(typ_t('a)))
  | Label(string)
  | TupLabel(typ_t('a), typ_t('a))
  | Parens(typ_t('a))
  | Ap(typ_t('a), typ_t('a))
  | Rec(tpat_t('a), typ_t('a))
  | Forall(tpat_t('a), typ_t('a))
and typ_t('a) = Annotated.t(typ_term('a), 'a)
and tpat_term('a) =
  | Invalid(string)
  | EmptyHole
  | MultiHole(list(any_t('a)))
  | Var(string)
and tpat_t('a) = Annotated.t(tpat_term('a), 'a)
and rul_term('a) =
  | Invalid(string)
  | Hole(list(any_t('a)))
  | Rules(exp_t('a), list((pat_t('a), exp_t('a))))
and rul_t('a) = Annotated.t(rul_term('a), 'a)
and environment_t('a) = VarBstMap.Ordered.t_(exp_t('a))
and closure_environment_t('a) = (Id.t, environment_t('a))
and stepper_filter_kind_t('a) =
  | Filter(filter('a))
  | Residue(int, FilterAction.t)
and type_hole('a) =
  | Invalid(string)
  | EmptyHole
  | MultiHole(list(any_t('a)))
and type_provenance('a) =
  | SynSwitch
  | Hole(type_hole('a))
  | Internal
and filter('a) = {
  pat: exp_t('a),
  act: FilterAction.t,
};

module IdTag = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    [@show.opaque]
    ids: list(Id.t),
    [@show.opaque]
    /* Exp invariant: copied should always be false, and the id should be unique
       DHExp invariant: if copied is true, then this term and its children may not
       have unique ids. The flag is used to avoid deep-copying expressions during
       evaluation, while keeping track of where we will need to replace the ids
       at the end of evaluation to keep them unique.*/
    copied: bool,
  };
};
