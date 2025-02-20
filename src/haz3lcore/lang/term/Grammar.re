open Util;

module Annotated = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t('a, 'b) = {
    term: 'a,
    annotation: 'b,
  };

  let term_of = x => x.term;
  let unwrap = x => (x.term, term' => {...x, term: term'});

  let empty = term => {term, annotation: ()};
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


let rec map_exp_annotation: type a b. (a => b, exp_t(a)) => exp_t(b) =
  (f, e) => (
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
      exp_t(b)
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

module type DefaultAnnotation = {
  type t;
  let default_value: unit => t;
};

module UnitAnnotation: DefaultAnnotation = {
  type t = unit;
  let default_value = () => ();
};

module Factory = (DefaultAnnotation: DefaultAnnotation) => {
  let invalid = (~ann=?, s): exp_t(DefaultAnnotation.t) => {
    term: Invalid(s),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let empty_hole = (~ann=?, ()): exp_t(DefaultAnnotation.t) => {
    term: EmptyHole,
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let multi_hole = (~ann=?, l): exp_t(DefaultAnnotation.t) => {
    term: MultiHole(l),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let dynamic_error_hole = (~ann=?, e, err): exp_t(DefaultAnnotation.t) => {
    term: DynamicErrorHole(e, err),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let failed_cast = (~ann=?, e, t1, t2): exp_t(DefaultAnnotation.t) => {
    term: FailedCast(e, t1, t2),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let deferral = (~ann=?, pos): exp_t(DefaultAnnotation.t) => {
    term: Deferral(pos),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let undefined = (~ann=?, ()): exp_t(DefaultAnnotation.t) => {
    term: Undefined,
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let bool = (~ann=?, b): exp_t(DefaultAnnotation.t) => {
    term: Bool(b),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let int = (~ann=?, i): exp_t(DefaultAnnotation.t) => {
    term: Int(i),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let float = (~ann=?, f): exp_t(DefaultAnnotation.t) => {
    term: Float(f),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let string = (~ann=?, s): exp_t(DefaultAnnotation.t) => {
    term: String(s),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let list_lit = (~ann=?, l): exp_t(DefaultAnnotation.t) => {
    term: ListLit(l),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let constructor = (~ann=?, s, t): exp_t(DefaultAnnotation.t) => {
    term: Constructor(s, t),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let fn = (~ann=?, p, e, t, v): exp_t(DefaultAnnotation.t) => {
    term: Fun(p, e, t, v),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let typ_fun = (~ann=?, p, e, v): exp_t(DefaultAnnotation.t) => {
    term: TypFun(p, e, v),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let tuple = (~ann=?, l): exp_t(DefaultAnnotation.t) => {
    term: Tuple(l),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let label = (~ann=?, l): exp_t(DefaultAnnotation.t) => {
    term: Label(l),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let tup_label = (~ann=?, l, e): exp_t(DefaultAnnotation.t) => {
    term: TupLabel(l, e),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let dot = (~ann=?, e1, e2): exp_t(DefaultAnnotation.t) => {
    term: Dot(e1, e2),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let var = (~ann=?, v): exp_t(DefaultAnnotation.t) => {
    term: Var(v),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let let_ = (~ann=?, p, e1, e2): exp_t(DefaultAnnotation.t) => {
    term: Let(p, e1, e2),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let fix_f = (~ann=?, p, e, env): exp_t(DefaultAnnotation.t) => {
    term: FixF(p, e, env),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let ty_alias = (~ann=?, p, t, e): exp_t(DefaultAnnotation.t) => {
    term: TyAlias(p, t, e),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let ap = (~ann=?, d, e1, e2): exp_t(DefaultAnnotation.t) => {
    term: Ap(d, e1, e2),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let typ_ap = (~ann=?, e, t): exp_t(DefaultAnnotation.t) => {
    term: TypAp(e, t),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let deferred_ap = (~ann=?, e, l): exp_t(DefaultAnnotation.t) => {
    term: DeferredAp(e, l),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let if_ = (~ann=?, e1, e2, e3): exp_t(DefaultAnnotation.t) => {
    term: If(e1, e2, e3),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let seq = (~ann=?, e1, e2): exp_t(DefaultAnnotation.t) => {
    term: Seq(e1, e2),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let test = (~ann=?, e): exp_t(DefaultAnnotation.t) => {
    term: Test(e),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let filter = (~ann=?, k, e): exp_t(DefaultAnnotation.t) => {
    term: Filter(k, e),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let closure = (~ann=?, env, e): exp_t(DefaultAnnotation.t) => {
    term: Closure(env, e),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let parens = (~ann=?, e): exp_t(DefaultAnnotation.t) => {
    term: Parens(e),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let cons = (~ann=?, e1, e2): exp_t(DefaultAnnotation.t) => {
    term: Cons(e1, e2),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let list_concat = (~ann=?, e1, e2): exp_t(DefaultAnnotation.t) => {
    term: ListConcat(e1, e2),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let un_op = (~ann=?, op, e): exp_t(DefaultAnnotation.t) => {
    term: UnOp(op, e),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let bin_op = (~ann=?, op, e1, e2): exp_t(DefaultAnnotation.t) => {
    term: BinOp(op, e1, e2),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let builtin_fun = (~ann=?, s): exp_t(DefaultAnnotation.t) => {
    term: BuiltinFun(s),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let match = (~ann=?, e, l): exp_t(DefaultAnnotation.t) => {
    term: Match(e, l),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let cast = (~ann=?, e, t1, t2): exp_t(DefaultAnnotation.t) => {
    term: Cast(e, t1, t2),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };

  // pat
  let pat_invalid = (~ann=?, s): pat_t(DefaultAnnotation.t) => {
    term: Invalid(s),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let pat_empty_hole = (~ann=?, ()): pat_t(DefaultAnnotation.t) => {
    term: EmptyHole,
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let pat_multi_hole = (~ann=?, l): pat_t(DefaultAnnotation.t) => {
    term: MultiHole(l),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let pat_wild = (~ann=?, ()): pat_t(DefaultAnnotation.t) => {
    term: Wild,
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let pat_int = (~ann=?, i): pat_t(DefaultAnnotation.t) => {
    term: Int(i),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let pat_float = (~ann=?, f): pat_t(DefaultAnnotation.t) => {
    term: Float(f),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let pat_bool = (~ann=?, b): pat_t(DefaultAnnotation.t) => {
    term: Bool(b),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let pat_string = (~ann=?, s): pat_t(DefaultAnnotation.t) => {
    term: String(s),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let pat_list_lit = (~ann=?, l): pat_t(DefaultAnnotation.t) => {
    term: ListLit(l),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let pat_constructor = (~ann=?, s, t): pat_t(DefaultAnnotation.t) => {
    term: Constructor(s, t),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let pat_cons = (~ann=?, p1, p2): pat_t(DefaultAnnotation.t) => {
    term: Cons(p1, p2),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let pat_var = (~ann=?, v): pat_t(DefaultAnnotation.t) => {
    term: Var(v),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let pat_tuple = (~ann=?, l): pat_t(DefaultAnnotation.t) => {
    term: Tuple(l),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let pat_label = (~ann=?, l): pat_t(DefaultAnnotation.t) => {
    term: Label(l),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let pat_tup_label = (~ann=?, p1, p2): pat_t(DefaultAnnotation.t) => {
    term: TupLabel(p1, p2),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let pat_parens = (~ann=?, p): pat_t(DefaultAnnotation.t) => {
    term: Parens(p),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let pat_ap = (~ann=?, p1, p2): pat_t(DefaultAnnotation.t) => {
    term: Ap(p1, p2),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let pat_cast = (~ann=?, p, t1, t2): pat_t(DefaultAnnotation.t) => {
    term: Cast(p, t1, t2),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };

  // typ
  let ty_unknown = (~ann=?, p): typ_t(DefaultAnnotation.t) => {
    term: Unknown(p),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let ty_int = (~ann=?, ()): typ_t(DefaultAnnotation.t) => {
    term: Int,
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let ty_float = (~ann=?, ()): typ_t(DefaultAnnotation.t) => {
    term: Float,
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let ty_bool = (~ann=?, ()): typ_t(DefaultAnnotation.t) => {
    term: Bool,
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let ty_string = (~ann=?, ()): typ_t(DefaultAnnotation.t) => {
    term: String,
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let ty_var = (~ann=?, s): typ_t(DefaultAnnotation.t) => {
    term: Var(s),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let ty_list = (~ann=?, t): typ_t(DefaultAnnotation.t) => {
    term: List(t),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let ty_arrow = (~ann=?, t1, t2): typ_t(DefaultAnnotation.t) => {
    term: Arrow(t1, t2),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let ty_sum = (~ann=?, m): typ_t(DefaultAnnotation.t) => {
    term: Sum(m),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let ty_prod = (~ann=?, l): typ_t(DefaultAnnotation.t) => {
    term: Prod(l),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let ty_label = (~ann=?, l): typ_t(DefaultAnnotation.t) => {
    term: Label(l),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let ty_tup_label = (~ann=?, t1, t2): typ_t(DefaultAnnotation.t) => {
    term: TupLabel(t1, t2),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let ty_parens = (~ann=?, t): typ_t(DefaultAnnotation.t) => {
    term: Parens(t),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let ty_ap = (~ann=?, t1, t2): typ_t(DefaultAnnotation.t) => {
    term: Ap(t1, t2),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let ty_rec = (~ann=?, tp, t): typ_t(DefaultAnnotation.t) => {
    term: Rec(tp, t),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let ty_forall = (~ann=?, tp, t): typ_t(DefaultAnnotation.t) => {
    term: Forall(tp, t),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };

  // tpat
  let tpat_invalid = (~ann=?, s): tpat_t(DefaultAnnotation.t) => {
    term: Invalid(s),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let tpat_empty_hole = (~ann=?, ()): tpat_t(DefaultAnnotation.t) => {
    term: EmptyHole,
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let tpat_multi_hole = (~ann=?, l): tpat_t(DefaultAnnotation.t) => {
    term: MultiHole(l),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let tpat_var = (~ann=?, s): tpat_t(DefaultAnnotation.t) => {
    term: Var(s),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };

  //rul
  let rul_invalid = (~ann=?, s): rul_t(DefaultAnnotation.t) => {
    term: Invalid(s),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let rul_hole = (~ann=?, l): rul_t(DefaultAnnotation.t) => {
    term: Hole(l),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };
  let rul_rules = (~ann=?, e, l): rul_t(DefaultAnnotation.t) => {
    term: Rules(e, l),
    annotation: Option.value(~default=DefaultAnnotation.default_value(), ann),
  };

  // environment
  let environment = (env): environment_t(DefaultAnnotation.t) => {
    VarBstMap.Ordered.mapo(((_, y)) => map_exp_annotation(x => x, y), env);
  };

  // closure_environment
  let closure_environment =
      (id, env): closure_environment_t(DefaultAnnotation.t) => {
    (id, environment(env));
  };

  // stepper_filter_kind
  let sf_filter = (f): stepper_filter_kind_t(DefaultAnnotation.t) => {
    Filter({pat: map_exp_annotation(x => x, f.pat), act: f.act});
  };
  let sf_residue = (i, act): stepper_filter_kind_t(DefaultAnnotation.t) => {
    Residue(i, act);
  };

  // type_hole
  let th_invalid = (s): type_hole(DefaultAnnotation.t) => {
    Invalid(s);
  };
  let th_empty_hole = (): type_hole(DefaultAnnotation.t) => {
    EmptyHole;
  };
  let th_multi_hole = (l): type_hole(DefaultAnnotation.t) => {
    MultiHole(l);
  };

  // type_provenance
  let tp_syn_switch = (): type_provenance(DefaultAnnotation.t) => {
    SynSwitch;
  };
  let tp_hole = (h): type_provenance(DefaultAnnotation.t) => {
    Hole(h);
  };
  let tp_internal = (): type_provenance(DefaultAnnotation.t) => {
    Internal;
  };
};
