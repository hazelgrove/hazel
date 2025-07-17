open Util;

module Annotated = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t('a, 'b) = {
    term: 'a,
    annotation: 'b,
  };
  /* uncomment to make terms pp without annotation */
  //   let pp:
  //     type a b.
  //       (
  //         (Format.formatter, a) => unit,
  //         (Format.formatter, b) => unit,
  //         Format.formatter,
  //         t(a, b)
  //       ) =>
  //       unit =
  //     (fmt_a, _, fmtr, t) => {
  //       fmt_a(fmtr, t.term);
  //     };

  let term_of = x => x.term;
  let unwrap = x => (
    x.term,
    term' => {
      ...x,
      term: term',
    },
  );

  let empty = term => {
    term,
    annotation: (),
  };
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
  | Deferral(deferral_position_t)
  | Undefined
  | Atom(Atom.t)
  | ListLit(list(exp_t('a)))
  /* The type double-option field of this constructor is required to assign the correct
     statics to constructors after evaluation. In dynamic expressions `Some(None)` means
     that it is a free constructor, while Some(Some(t)) means it has type t. In user expressions
     this field is None.*/
  | Constructor(string, option(option(typ_t('a))))
  | Fun(pat_t('a), exp_t('a), option(typ_t('a)), option(Var.t)) // typ_t field is only used to display types in results
  | TypFun(tpat_t('a), exp_t('a), option(Var.t))
  | Tuple(list(exp_t('a)))
  | Label(string)
  | TupLabel(exp_t('a), exp_t('a))
  | Dot(exp_t('a), exp_t('a))
  | LivelitName(string)
  | Var(Var.t)
  | Let(pat_t('a), exp_t('a), exp_t('a))
  | FixF(pat_t('a), exp_t('a), option(closure_environment_t('a)))
  | TyAlias(tpat_t('a), typ_t('a), exp_t('a))
  | Use(typ_t('a), exp_t('a))
  | Ap(Operators.ap_direction, exp_t('a), exp_t('a))
  | TypAp(exp_t('a), typ_t('a))
  | DeferredAp(exp_t('a), list(exp_t('a)))
  | If(exp_t('a), exp_t('a), exp_t('a))
  | Seq(exp_t('a), exp_t('a))
  | Test(exp_t('a))
  | HintedTest(exp_t('a), exp_t('a))
  | Filter(stepper_filter_kind_t('a), exp_t('a))
  | Closure([@show.opaque] closure_environment_t('a), exp_t('a))
  | Parens(exp_t('a)) // (
  | Probe(exp_t('a), Probe.t)
  | Cons(exp_t('a), exp_t('a))
  | ListConcat(exp_t('a), exp_t('a))
  | UnOp(Operators.op_un, exp_t('a))
  | BinOp(Operators.op_bin, exp_t('a), exp_t('a))
  | BuiltinFun(string)
  | Match(exp_t('a), list((pat_t('a), exp_t('a))))
  | Asc(exp_t('a), typ_t('a))
and exp_t('a) = Annotated.t(exp_term('a), 'a)
and pat_term('a) =
  | Invalid(string)
  | EmptyHole
  | MultiHole(list(any_t('a)))
  | Wild
  | Atom(Atom.t)
  | ListLit(list(pat_t('a)))
  | Constructor(string, option(option(typ_t('a)))) // see comment on constructor expressions
  | Cons(pat_t('a), pat_t('a))
  | Var(Var.t)
  | Tuple(list(pat_t('a)))
  | Label(string)
  | TupLabel(pat_t('a), pat_t('a))
  | Parens(pat_t('a))
  | Probe(pat_t('a), Probe.t)
  | Ap(pat_t('a), pat_t('a))
  | Asc(pat_t('a), typ_t('a))
and pat_t('a) = Annotated.t(pat_term('a), 'a)
and typ_term('a) =
  | Unknown(type_provenance('a))
  | Atom(Atom.cls)
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
  | MultiHole(list(any_t('a)))
  | Rules(exp_t('a), list((pat_t('a), exp_t('a))))
and rul_t('a) = Annotated.t(rul_term('a), 'a)
and environment_t('a) = VarBstMap.Ordered.t_(exp_t('a))
and closure_environment_t('a) = {
  id: Id.t,
  env: environment_t('a),
  call_stack: Probe.call_stack,
}
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
        | Deferral(pos) => Deferral(pos)
        | Undefined => Undefined
        | Atom(c) => Atom(c)
        | LivelitName(s) => LivelitName(s)
        | ListLit(l) => ListLit(List.map(x => map_exp_annotation(f, x), l))
        | Constructor(s, t) =>
          Constructor(s, Option.map(Option.map(map_typ_annotation(f)), t))
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
        | Use(t, e) =>
          Use(map_typ_annotation(f, t), map_exp_annotation(f, e))
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
        | HintedTest(e1, h) =>
          HintedTest(map_exp_annotation(f, e1), map_exp_annotation(f, h))
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
        | Probe(e, probe) => Probe(map_exp_annotation(f, e), probe)
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
        | Asc(e, t) =>
          Asc(map_exp_annotation(f, e), map_typ_annotation(f, t))
        };
      {
        term,
        annotation: new_annotation,
      };
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
        | Atom(c) => Atom(c)
        | ListLit(l) => ListLit(List.map(x => map_pat_annotation(f, x), l))
        | Constructor(s, t) =>
          Constructor(s, Option.map(Option.map(map_typ_annotation(f)), t))
        | Cons(p1, p2) =>
          Cons(map_pat_annotation(f, p1), map_pat_annotation(f, p2))
        | Var(v) => Var(v)
        | Tuple(l) => Tuple(List.map(x => map_pat_annotation(f, x), l))
        | Label(l) => Label(l)
        | TupLabel(p1, p2) =>
          TupLabel(map_pat_annotation(f, p1), map_pat_annotation(f, p2))
        | Parens(p) => Parens(map_pat_annotation(f, p))
        | Probe(p, probe) => Probe(map_pat_annotation(f, p), probe)
        | Ap(p1, p2) =>
          Ap(map_pat_annotation(f, p1), map_pat_annotation(f, p2))
        | Asc(p, t) =>
          Asc(map_pat_annotation(f, p), map_typ_annotation(f, t))
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
        | Atom(c) => Atom(c)
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
        | MultiHole(l) =>
          MultiHole(List.map(x => map_any_annotation(f, x), l))
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
      Filter({
        pat: map_exp_annotation(f, filter.pat),
        act: filter.act,
      })
    | Residue(i, act) => Residue(i, act)
    };
  }
and map_closure_environment_annotation:
  type a b. (a => b, closure_environment_t(a)) => closure_environment_t(b) =
  (f, {id, env, call_stack}) => {
    id,
    env: VarBstMap.Ordered.mapo(((_, y)) => map_exp_annotation(f, y), env),
    call_stack,
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

module Factory = (DefaultAnnotation: DefaultAnnotation) => {
  type exp = exp_t(DefaultAnnotation.t);
  type pat = pat_t(DefaultAnnotation.t);
  type typ = typ_t(DefaultAnnotation.t);
  type tpat = tpat_t(DefaultAnnotation.t);
  type typ_provenance = type_provenance(DefaultAnnotation.t);

  let default_annotation = ann =>
    Option.value(~default=DefaultAnnotation.default_value(), ann);
  module Exp = {
    type t = exp_t(DefaultAnnotation.t);
    let invalid = (~ann=?, s): exp_t(DefaultAnnotation.t) => {
      term: Invalid(s),
      annotation: default_annotation(ann),
    };
    let empty_hole = (~ann=?, ()): exp_t(DefaultAnnotation.t) => {
      term: EmptyHole,
      annotation: default_annotation(ann),
    };
    let multi_hole = (~ann=?, l): exp_t(DefaultAnnotation.t) => {
      term: MultiHole(l),
      annotation: default_annotation(ann),
    };
    let dynamic_error_hole = (~ann=?, e, err): exp_t(DefaultAnnotation.t) => {
      term: DynamicErrorHole(e, err),
      annotation: default_annotation(ann),
    };
    let deferral = (~ann=?, pos): exp_t(DefaultAnnotation.t) => {
      term: Deferral(pos),
      annotation: default_annotation(ann),
    };
    let undefined = (~ann=?, ()): exp_t(DefaultAnnotation.t) => {
      term: Undefined,
      annotation: default_annotation(ann),
    };
    let basic = (~ann=?, c): exp_t(DefaultAnnotation.t) => {
      term: Atom(c),
      annotation: default_annotation(ann),
    };
    let bool = (~ann=?, b): exp_t(DefaultAnnotation.t) => {
      term: Atom(Bool(b)),
      annotation: default_annotation(ann),
    };
    let big_int = (~ann=?, i: Bigint.t): exp_t(DefaultAnnotation.t) => {
      term: Atom(Int(i)),
      annotation: default_annotation(ann),
    };
    let int = (~ann=?, i: Int.t): exp_t(DefaultAnnotation.t) =>
      big_int(~ann?, Bigint.of_int(i));
    let sint = (~ann=?, i): exp_t(DefaultAnnotation.t) => {
      term: Atom(SInt(i)),
      annotation: default_annotation(ann),
    };
    let float = (~ann=?, f): exp_t(DefaultAnnotation.t) => {
      term: Atom(Float(f)),
      annotation: default_annotation(ann),
    };
    let string = (~ann=?, s): exp_t(DefaultAnnotation.t) => {
      term: Atom(String(s)),
      annotation: default_annotation(ann),
    };
    let nat = (~ann=?, i): exp_t(DefaultAnnotation.t) => {
      term: Atom(Nat(i)),
      annotation: default_annotation(ann),
    };
    let list_lit = (~ann=?, l): exp_t(DefaultAnnotation.t) => {
      term: ListLit(l),
      annotation: default_annotation(ann),
    };
    let constructor = (~ann=?, s, t): exp_t(DefaultAnnotation.t) => {
      term: Constructor(s, t),
      annotation: default_annotation(ann),
    };
    let fn = (~ann=?, p, e, t, v): exp_t(DefaultAnnotation.t) => {
      term: Fun(p, e, t, v),
      annotation: default_annotation(ann),
    };
    let typ_fun = (~ann=?, p, e, v): exp_t(DefaultAnnotation.t) => {
      term: TypFun(p, e, v),
      annotation: default_annotation(ann),
    };
    let tuple = (~ann=?, l): exp_t(DefaultAnnotation.t) => {
      term: Tuple(l),
      annotation: default_annotation(ann),
    };
    let label = (~ann=?, l): exp_t(DefaultAnnotation.t) => {
      term: Label(l),
      annotation: default_annotation(ann),
    };
    let tup_label = (~ann=?, l, e): exp_t(DefaultAnnotation.t) => {
      term: TupLabel(l, e),
      annotation: default_annotation(ann),
    };
    let dot = (~ann=?, e1, e2): exp_t(DefaultAnnotation.t) => {
      term: Dot(e1, e2),
      annotation: default_annotation(ann),
    };
    let var = (~ann=?, v): exp_t(DefaultAnnotation.t) => {
      term: Var(v),
      annotation: default_annotation(ann),
    };
    let livelit_name = (~ann=?, s): exp_t(DefaultAnnotation.t) => {
      term: LivelitName(s),
      annotation: default_annotation(ann),
    };
    let livelit_ap = (~ann=?, d, e1, e2): exp_t(DefaultAnnotation.t) => {
      term: Ap(d, e1, e2),
      annotation: default_annotation(ann),
    };
    let let_ = (~ann=?, p, e1, e2): exp_t(DefaultAnnotation.t) => {
      term: Let(p, e1, e2),
      annotation: default_annotation(ann),
    };
    let fix_f = (~ann=?, p, e, env): exp_t(DefaultAnnotation.t) => {
      term: FixF(p, e, env),
      annotation: default_annotation(ann),
    };
    let ty_alias = (~ann=?, p, t, e): exp_t(DefaultAnnotation.t) => {
      term: TyAlias(p, t, e),
      annotation: default_annotation(ann),
    };
    let use = (~ann=?, t, e): exp_t(DefaultAnnotation.t) => {
      term: Use(t, e),
      annotation: default_annotation(ann),
    };
    let ap = (~ann=?, d, e1, e2): exp_t(DefaultAnnotation.t) => {
      term: Ap(d, e1, e2),
      annotation: default_annotation(ann),
    };
    let typ_ap = (~ann=?, e, t): exp_t(DefaultAnnotation.t) => {
      term: TypAp(e, t),
      annotation: default_annotation(ann),
    };
    let deferred_ap = (~ann=?, e, l): exp_t(DefaultAnnotation.t) => {
      term: DeferredAp(e, l),
      annotation: default_annotation(ann),
    };
    let if_ = (~ann=?, e1, e2, e3): exp_t(DefaultAnnotation.t) => {
      term: If(e1, e2, e3),
      annotation: default_annotation(ann),
    };
    let seq = (~ann=?, e1, e2): exp_t(DefaultAnnotation.t) => {
      term: Seq(e1, e2),
      annotation: default_annotation(ann),
    };
    let test = (~ann=?, e): exp_t(DefaultAnnotation.t) => {
      term: Test(e),
      annotation: default_annotation(ann),
    };
    let hinted_test = (~ann=?, e, h): exp_t(DefaultAnnotation.t) => {
      term: HintedTest(e, h),
      annotation: default_annotation(ann),
    };
    let filter = (~ann=?, k, e): exp_t(DefaultAnnotation.t) => {
      term: Filter(k, e),
      annotation: default_annotation(ann),
    };
    let closure = (~ann=?, env, e): exp_t(DefaultAnnotation.t) => {
      term: Closure(env, e),
      annotation: default_annotation(ann),
    };
    let parens = (~ann=?, e): exp_t(DefaultAnnotation.t) => {
      term: Parens(e),
      annotation: default_annotation(ann),
    };
    let probe = (~ann=?, e1, e2): exp_t(DefaultAnnotation.t) => {
      term: Probe(e1, e2),
      annotation: default_annotation(ann),
    };
    let cons = (~ann=?, e1, e2): exp_t(DefaultAnnotation.t) => {
      term: Cons(e1, e2),
      annotation: default_annotation(ann),
    };
    let list_concat = (~ann=?, e1, e2): exp_t(DefaultAnnotation.t) => {
      term: ListConcat(e1, e2),
      annotation: default_annotation(ann),
    };
    let un_op = (~ann=?, op, e): exp_t(DefaultAnnotation.t) => {
      term: UnOp(op, e),
      annotation: default_annotation(ann),
    };
    let bin_op = (~ann=?, op, e1, e2): exp_t(DefaultAnnotation.t) => {
      term: BinOp(op, e1, e2),
      annotation: default_annotation(ann),
    };
    let builtin_fun = (~ann=?, s): exp_t(DefaultAnnotation.t) => {
      term: BuiltinFun(s),
      annotation: default_annotation(ann),
    };
    let match = (~ann=?, e, l): exp_t(DefaultAnnotation.t) => {
      term: Match(e, l),
      annotation: default_annotation(ann),
    };
    let asc = (~ann=?, e, t): exp_t(DefaultAnnotation.t) => {
      term: Asc(e, t),
      annotation: default_annotation(ann),
    };
  };
  module Pat = {
    let invalid = (~ann=?, s): pat_t(DefaultAnnotation.t) => {
      term: Invalid(s),
      annotation: default_annotation(ann),
    };
    let empty_hole = (~ann=?, ()): pat_t(DefaultAnnotation.t) => {
      term: EmptyHole,
      annotation: default_annotation(ann),
    };
    let multi_hole = (~ann=?, l): pat_t(DefaultAnnotation.t) => {
      term: MultiHole(l),
      annotation: default_annotation(ann),
    };
    let wild = (~ann=?, ()): pat_t(DefaultAnnotation.t) => {
      term: Wild,
      annotation: default_annotation(ann),
    };
    let basic = (~ann=?, c): pat_t(DefaultAnnotation.t) => {
      term: Atom(c),
      annotation: default_annotation(ann),
    };
    let big_int = (~ann=?, i): pat_t(DefaultAnnotation.t) => {
      term: Atom(Int(i)),
      annotation: default_annotation(ann),
    };
    let int = (~ann=?, i): pat_t(DefaultAnnotation.t) =>
      big_int(~ann?, Bigint.of_int(i));
    let sint = (~ann=?, i): pat_t(DefaultAnnotation.t) => {
      term: Atom(SInt(i)),
      annotation: default_annotation(ann),
    };
    let float = (~ann=?, f): pat_t(DefaultAnnotation.t) => {
      term: Atom(Float(f)),
      annotation: default_annotation(ann),
    };
    let bool = (~ann=?, b): pat_t(DefaultAnnotation.t) => {
      term: Atom(Bool(b)),
      annotation: default_annotation(ann),
    };
    let string = (~ann=?, s): pat_t(DefaultAnnotation.t) => {
      term: Atom(String(s)),
      annotation: default_annotation(ann),
    };
    let nat = (~ann=?, i): pat_t(DefaultAnnotation.t) => {
      term: Atom(Nat(i)),
      annotation: default_annotation(ann),
    };
    let list_lit = (~ann=?, l): pat_t(DefaultAnnotation.t) => {
      term: ListLit(l),
      annotation: default_annotation(ann),
    };
    let constructor = (~ann=?, s, t): pat_t(DefaultAnnotation.t) => {
      term: Constructor(s, t),
      annotation: default_annotation(ann),
    };
    let cons = (~ann=?, p1, p2): pat_t(DefaultAnnotation.t) => {
      term: Cons(p1, p2),
      annotation: default_annotation(ann),
    };
    let var = (~ann=?, v): pat_t(DefaultAnnotation.t) => {
      term: Var(v),
      annotation: default_annotation(ann),
    };
    let tuple = (~ann=?, l): pat_t(DefaultAnnotation.t) => {
      term: Tuple(l),
      annotation: default_annotation(ann),
    };
    let label = (~ann=?, l): pat_t(DefaultAnnotation.t) => {
      term: Label(l),
      annotation: default_annotation(ann),
    };
    let tup_label = (~ann=?, p1, p2): pat_t(DefaultAnnotation.t) => {
      term: TupLabel(p1, p2),
      annotation: default_annotation(ann),
    };
    let parens = (~ann=?, p): pat_t(DefaultAnnotation.t) => {
      term: Parens(p),
      annotation: default_annotation(ann),
    };
    let probe = (~ann=?, p1, p2): pat_t(DefaultAnnotation.t) => {
      term: Probe(p1, p2),
      annotation: default_annotation(ann),
    };
    let ap = (~ann=?, p1, p2): pat_t(DefaultAnnotation.t) => {
      term: Ap(p1, p2),
      annotation: default_annotation(ann),
    };

    let asc = (~ann=?, p, t): pat_t(DefaultAnnotation.t) => {
      term: Asc(p, t),
      annotation: default_annotation(ann),
    };
  };

  module Typ = {
    let unknown = (~ann=?, p): typ_t(DefaultAnnotation.t) => {
      term: Unknown(p),
      annotation: default_annotation(ann),
    };
    let int = (~ann=?, ()): typ_t(DefaultAnnotation.t) => {
      term: Atom(Int),
      annotation: default_annotation(ann),
    };
    let sint = (~ann=?, ()): typ_t(DefaultAnnotation.t) => {
      term: Atom(SInt),
      annotation: default_annotation(ann),
    };
    let float = (~ann=?, ()): typ_t(DefaultAnnotation.t) => {
      term: Atom(Float),
      annotation: default_annotation(ann),
    };
    let bool = (~ann=?, ()): typ_t(DefaultAnnotation.t) => {
      term: Atom(Bool),
      annotation: default_annotation(ann),
    };
    let string = (~ann=?, ()): typ_t(DefaultAnnotation.t) => {
      term: Atom(String),
      annotation: default_annotation(ann),
    };
    let nat = (~ann=?, ()): typ_t(DefaultAnnotation.t) => {
      term: Atom(Nat),
      annotation: default_annotation(ann),
    };
    let var = (~ann=?, s): typ_t(DefaultAnnotation.t) => {
      term: Var(s),
      annotation: default_annotation(ann),
    };
    let list = (~ann=?, t): typ_t(DefaultAnnotation.t) => {
      term: List(t),
      annotation: default_annotation(ann),
    };
    let arrow = (~ann=?, t1, t2): typ_t(DefaultAnnotation.t) => {
      term: Arrow(t1, t2),
      annotation: default_annotation(ann),
    };
    let sum = (~ann=?, m): typ_t(DefaultAnnotation.t) => {
      term: Sum(m),
      annotation: default_annotation(ann),
    };
    let prod = (~ann=?, l): typ_t(DefaultAnnotation.t) => {
      term: Prod(l),
      annotation: default_annotation(ann),
    };
    let label = (~ann=?, l): typ_t(DefaultAnnotation.t) => {
      term: Label(l),
      annotation: default_annotation(ann),
    };
    let tup_label = (~ann=?, t1, t2): typ_t(DefaultAnnotation.t) => {
      term: TupLabel(t1, t2),
      annotation: default_annotation(ann),
    };
    let parens = (~ann=?, t): typ_t(DefaultAnnotation.t) => {
      term: Parens(t),
      annotation: default_annotation(ann),
    };
    let ap = (~ann=?, t1, t2): typ_t(DefaultAnnotation.t) => {
      term: Ap(t1, t2),
      annotation: default_annotation(ann),
    };
    let rec_ = (~ann=?, tp, t): typ_t(DefaultAnnotation.t) => {
      term: Rec(tp, t),
      annotation: default_annotation(ann),
    };
    let forall = (~ann=?, tp, t): typ_t(DefaultAnnotation.t) => {
      term: Forall(tp, t),
      annotation: default_annotation(ann),
    };
    let empty_hole = (~ann=?, ()): typ_t(DefaultAnnotation.t) => {
      term: Unknown(Hole(EmptyHole)),
      annotation: default_annotation(ann),
    };
  };

  module TPat = {
    let invalid = (~ann=?, s): tpat_t(DefaultAnnotation.t) => {
      term: Invalid(s),
      annotation: default_annotation(ann),
    };
    let empty_hole = (~ann=?, ()): tpat_t(DefaultAnnotation.t) => {
      term: EmptyHole,
      annotation: default_annotation(ann),
    };
    let multi_hole = (~ann=?, l): tpat_t(DefaultAnnotation.t) => {
      term: MultiHole(l),
      annotation: default_annotation(ann),
    };
    let var = (~ann=?, s): tpat_t(DefaultAnnotation.t) => {
      term: Var(s),
      annotation: default_annotation(ann),
    };
  };

  module Rul = {
    let rul_invalid = (~ann=?, s): rul_t(DefaultAnnotation.t) => {
      term: Invalid(s),
      annotation: default_annotation(ann),
    };
    let rul_hole = (~ann=?, l): rul_t(DefaultAnnotation.t) => {
      term: MultiHole(l),
      annotation: default_annotation(ann),
    };
    let rul_rules = (~ann=?, e, l): rul_t(DefaultAnnotation.t) => {
      term: Rules(e, l),
      annotation: default_annotation(ann),
    };
  };

  let environment = (env): environment_t(DefaultAnnotation.t) => {
    VarBstMap.Ordered.mapo(((_, y)) => map_exp_annotation(x => x, y), env);
  };

  let closure_environment =
      (~callstack, id, env): closure_environment_t(DefaultAnnotation.t) => {
    id,
    env: environment(env),
    call_stack: callstack,
  };

  module StepperFilter = {
    let filter = (f): stepper_filter_kind_t(DefaultAnnotation.t) => {
      Filter({
        pat: map_exp_annotation(x => x, f.pat),
        act: f.act,
      });
    };
    let residue = (i, act): stepper_filter_kind_t(DefaultAnnotation.t) => {
      Residue(i, act);
    };
  };

  module TypeHole = {
    let invalid = (s): type_hole(DefaultAnnotation.t) => {
      Invalid(s);
    };
    let empty_hole = (): type_hole(DefaultAnnotation.t) => {
      EmptyHole;
    };
    let multi_hole = (l): type_hole(DefaultAnnotation.t) => {
      MultiHole(l);
    };
  };

  module TypeProvenance = {
    let syn_switch = (): type_provenance(DefaultAnnotation.t) => {
      SynSwitch;
    };
    let hole = (h): type_provenance(DefaultAnnotation.t) => {
      Hole(h);
    };
    let internal = (): type_provenance(DefaultAnnotation.t) => {
      Internal;
    };
  };
};

module UnitGrammar =
  Factory({
    type t = unit;
    let default_value = () => ();
  });
