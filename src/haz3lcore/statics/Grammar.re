open Util;

module Annotated = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t('a, 'b) = {
    term: 'a,
    annotation: 'b,
  };

  let term_of = x => x.term;
  let unwrap = x => (x.term, term' => {...x, term: term'});
};

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type deferral_position_t =
  | InAp
  | OutsideAp;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
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


let rec map_exp_annotation: type a b. (a => b, exp_t(a)) => exp_t(b) =
  (type a, type b, f: a => b, e: exp_t(a)) => (
    {
      let (term, annotation) = (e.term, e.annotation);
      let new_annotation: b = f(annotation);
      let term: exp_term(b) =
        switch (term) {
        | Invalid(s) => Invalid(s)
        | EmptyHole => EmptyHole
        | MultiHole(l) =>
          MultiHole(List.map(x => map_any_annotation(f, x), l))
        | DynamicErrorHole(e, err) =>
          DynamicErrorHole(map_exp_annotation(f, e), err)
        | FailedCast(e: exp_t(a), t1, t2) =>
          FailedCast(
            map_exp_annotation(f, e),
            map_typ_annotation(f, t1),
            map_typ_annotation(f, t2),
          )
        | Deferral(pos) => Deferral(pos)
        | Undefined => Undefined
        | Bool(b) => Bool(b)
        | Int(i) => Int(i)
        | Float(f) => Float(f)
        | String(s) => String(s)
        | ListLit(l) => ListLit(List.map(x => map_exp_annotation(f, x), l))
        | Constructor(s, t) => Constructor(s, map_typ_annotation(f, t))
        | Fun(p, e, t, v) =>
          Fun(
            map_pat_annotation(f, p),
            map_exp_annotation(f, e),
            Option.map(x => map_typ_annotation(f, x), t),
            Option.map(x => x, v),
          )
        | TypFun(p, e, v) =>
          TypFun(map_tpat_annotation(f, p), map_exp_annotation(f, e), v)
        | Tuple(l) => Tuple(List.map(x => map_exp_annotation(f, x), l))
        | Label(l) => Label(l)
        | TupLabel(l, e) =>
          TupLabel(map_exp_annotation(f, l), map_exp_annotation(f, e))
        | Dot(e1, e2) =>
          Dot(map_exp_annotation(f, e1), map_exp_annotation(f, e2))
        | Var(v) => Var(v)
        | Let(p, e1, e2) =>
          Let(
            map_pat_annotation(f, p),
            map_exp_annotation(f, e1),
            map_exp_annotation(f, e2),
          )
        | FixF(p, e, _) =>
          FixF(map_pat_annotation(f, p), map_exp_annotation(f, e), None)
        | TyAlias(p, t, e) =>
          TyAlias(
            map_tpat_annotation(f, p),
            map_typ_annotation(f, t),
            map_exp_annotation(f, e),
          )
        | Ap(d, e1, e2) =>
          Ap(d, map_exp_annotation(f, e1), map_exp_annotation(f, e2))
        | TypAp(e, t) =>
          TypAp(map_exp_annotation(f, e), map_typ_annotation(f, t))
        | DeferredAp(e, l) =>
          DeferredAp(
            map_exp_annotation(f, e),
            List.map(x => map_exp_annotation(f, x), l),
          )
        | If(e1, e2, e3) =>
          If(
            map_exp_annotation(f, e1),
            map_exp_annotation(f, e2),
            map_exp_annotation(f, e3),
          )
        | Seq(e1, e2) =>
          Seq(map_exp_annotation(f, e1), map_exp_annotation(f, e2))
        | Test(e) => Test(map_exp_annotation(f, e))
        | Filter(k, e) =>
          Filter(
            map_stepper_filter_kind_annotation(f, k),
            map_exp_annotation(f, e),
          )
        | Closure(env, e) =>
          Closure(
            map_closure_environment_annotation(f, env),
            map_exp_annotation(f, e),
          )
        | Parens(e) => Parens(map_exp_annotation(f, e))
        | Cons(e1, e2) =>
          Cons(map_exp_annotation(f, e1), map_exp_annotation(f, e2))
        | ListConcat(e1, e2) =>
          ListConcat(map_exp_annotation(f, e1), map_exp_annotation(f, e2))
        | UnOp(op, e) => UnOp(op, map_exp_annotation(f, e))
        | BinOp(op, e1, e2) =>
          BinOp(op, map_exp_annotation(f, e1), map_exp_annotation(f, e2))
        | BuiltinFun(s) => BuiltinFun(s)
        | Match(e, l) =>
          Match(
            map_exp_annotation(f, e),
            List.map(
              ((p, e)) =>
                (map_pat_annotation(f, p), map_exp_annotation(f, e)),
              l,
            ),
          )
        | Cast(e, t1, t2) =>
          Cast(
            map_exp_annotation(f, e),
            map_typ_annotation(f, t1),
            map_typ_annotation(f, t2),
          )
        };
      {term, annotation: new_annotation};
    }:
      exp_t(b) // TODO
  )

and map_any_annotation: 'a 'b. ('a => 'b, any_t('a)) => any_t('b) =
  (f, e) => {
    switch (e) {
    | Exp(e) => Exp(map_exp_annotation(f, e))
    | Pat(p) => Pat(map_pat_annotation(f, p))
    | Typ(t) => Typ(map_typ_annotation(f, t))
    | TPat(tp) => TPat(map_tpat_annotation(f, tp))
    | Rul(r) => Rul(map_rul_annotation(f, r))
    | Any(_) => Any()
    };
  }
and map_pat_annotation: 'a 'b. ('a => 'b, pat_t('a)) => pat_t('b) =
  (f, e) => {
    let (term, annotation) = (e.term, e.annotation);
    let new_annotation = f(annotation);
    {
      term:
        switch (term) {
        | Invalid(s) => Invalid(s)
        | EmptyHole => EmptyHole
        | MultiHole(l) =>
          MultiHole(List.map(x => map_any_annotation(f, x), l))
        | Wild => Wild
        | Int(i) => Int(i)
        | Float(f) => Float(f)
        | Bool(b) => Bool(b)
        | String(s) => String(s)
        | ListLit(l) => ListLit(List.map(x => map_pat_annotation(f, x), l))
        | Constructor(s, t) => Constructor(s, map_typ_annotation(f, t))
        | Cons(p1, p2) =>
          Cons(map_pat_annotation(f, p1), map_pat_annotation(f, p2))
        | Var(v) => Var(v)
        | Tuple(l) => Tuple(List.map(x => map_pat_annotation(f, x), l))
        | Label(l) => Label(l)
        | TupLabel(p1, p2) =>
          TupLabel(map_pat_annotation(f, p1), map_pat_annotation(f, p2))
        | Parens(p) => Parens(map_pat_annotation(f, p))
        | Ap(p1, p2) =>
          Ap(map_pat_annotation(f, p1), map_pat_annotation(f, p2))
        | Cast(p, t1, t2) =>
          Cast(
            map_pat_annotation(f, p),
            map_typ_annotation(f, t1),
            map_typ_annotation(f, t2),
          )
        },
      annotation: new_annotation,
    };
  }
and map_typ_annotation: 'a 'b. ('a => 'b, typ_t('a)) => typ_t('b) =
  (f, e) => {
    let (term, annotation) = (e.term, e.annotation);
    let new_annotation = f(annotation);
    {
      term:
        switch (term) {
        | Unknown(p) => Unknown(map_type_provenance_annotation(f, p))
        | Int => Int
        | Float => Float
        | Bool => Bool
        | String => String
        | Var(s) => Var(s)
        | List(t) => List(map_typ_annotation(f, t))
        | Arrow(t1, t2) =>
          Arrow(map_typ_annotation(f, t1), map_typ_annotation(f, t2))
        | Parens(t) => Parens(map_typ_annotation(f, t))
        | Ap(t1, t2) =>
          Ap(map_typ_annotation(f, t1), map_typ_annotation(f, t2))
        | Rec(tp, t) =>
          Rec(map_tpat_annotation(f, tp), map_typ_annotation(f, t))
        | Forall(tp, t) =>
          Forall(map_tpat_annotation(f, tp), map_typ_annotation(f, t))
        | Prod(l) => Prod(List.map(x => map_typ_annotation(f, x), l))
        | Label(l) => Label(l)
        | TupLabel(t1, t2) =>
          TupLabel(map_typ_annotation(f, t1), map_typ_annotation(f, t2))
        | Sum(m) =>
          Sum(ConstructorMap.map_preserving(map_typ_annotation(f), m))
        },
      annotation: new_annotation,
    };
  }
and map_tpat_annotation: 'a 'b. ('a => 'b, tpat_t('a)) => tpat_t('b) =
  (f, e) => {
    let (term, annotation) = (e.term, e.annotation);
    let new_annotation = f(annotation);
    {
      term:
        switch (term) {
        | Invalid(s) => Invalid(s)
        | EmptyHole => EmptyHole
        | MultiHole(l) =>
          MultiHole(List.map(x => map_any_annotation(f, x), l))
        | Var(s) => Var(s)
        },
      annotation: new_annotation,
    };
  }
and map_rul_annotation: 'a 'b. ('a => 'b, rul_t('a)) => rul_t('b) =
  (f, e) => {
    let (term, annotation) = (e.term, e.annotation);
    let new_annotation = f(annotation);
    {
      term:
        switch (term) {
        | Invalid(s) => Invalid(s)
        | Hole(l) => Hole(List.map(x => map_any_annotation(f, x), l))
        | Rules(e, l) =>
          Rules(
            map_exp_annotation(f, e),
            List.map(
              ((p, e)) =>
                (map_pat_annotation(f, p), map_exp_annotation(f, e)),
              l,
            ),
          )
        },
      annotation: new_annotation,
    };
  }
and map_stepper_filter_kind_annotation:
  'a 'b.
  ('a => 'b, stepper_filter_kind_t('a)) => stepper_filter_kind_t('b)
 =
  (f, e) => {
    switch (e) {
    | Filter(filter) =>
      Filter({pat: map_exp_annotation(f, filter.pat), act: filter.act})
    | Residue(i, act) => Residue(i, act)
    };
  }
and map_closure_environment_annotation:
  type a b. (a => b, closure_environment_t(a)) => closure_environment_t(b) =
  (f, (id, env)) => {
    (
      id,
      VarBstMap.Ordered.mapo(((_, y)) => map_exp_annotation(f, y), env),
    );
  }

and map_type_provenance_annotation:
  'a 'b.
  ('a => 'b, type_provenance('a)) => type_provenance('b)
 =
  (f, e) => {
    switch (e) {
    | SynSwitch => SynSwitch
    | Hole(h) => Hole(map_type_hole_annotation(f, h))
    | Internal => Internal
    };
  }
and map_type_hole_annotation:
  'a 'b.
  ('a => 'b, type_hole('a)) => type_hole('b)
 =
  (f, e) => {
    switch (e) {
    | Invalid(s) => Invalid(s)
    | EmptyHole => EmptyHole
    | MultiHole(l) => MultiHole(List.map(x => map_any_annotation(f, x), l))
    };
  };
