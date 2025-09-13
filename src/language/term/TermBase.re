let continue = x => x;
let stop = (_, x) => x;

/*
   This megafile contains the definitions of the expression data types in
   Hazel. They are all in one file because they are mutually recursive, and
   OCaml doesn't let us have mutually recursive files. Any definition that
   is not mutually recursive across the whole data structure should be
   defined in Any.re, Exp.re, Typ.re, Pat.re, TPat.re, etc...

   Each module has:

   - A type definition for the term

   - A map_term function that allows you to apply a function to every term in
     the data structure with the following type:

     map_term:
     (
       ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
       ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
       ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
       ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
       ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
       ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
       t
     ) =>
     t;

     Each argument to `map_term` specifies what should happen at each node in the
     data structure. Each function takes two arguments: a `continue` function that
     allows the map to continue on all the children nodes, and the current node
     itself. If you don't explicitly call the `continue` function, the map will
     not traverse the children nodes. If you don't provide a function for a
     specific kind of node, the map will simply continue at that node without
     any additional action.

   - A fast_equal function that compares two terms for equality, it performs
     structural equality except for the case of closures, where it just compares
     the id of the closure.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type any_t = Grammar.any_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type exp_t = Grammar.exp_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type exp_term = Grammar.exp_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type pat_t = Grammar.pat_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type pat_term = Grammar.pat_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type typ_t = Grammar.typ_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type typ_term = Grammar.typ_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type tpat_t = Grammar.tpat_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type tpat_term = Grammar.tpat_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type rul_t = Grammar.rul_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type rul_term = Grammar.rul_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type environment_t = Grammar.environment_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type closure_environment_t = Grammar.closure_environment_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type stepper_filter_kind_t = Grammar.stepper_filter_kind_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type type_hole = Grammar.type_hole(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type type_provenance = Grammar.type_provenance(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type filter = Grammar.filter(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type deferral_position_t = Grammar.deferral_position_t;

module rec Any: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = any_t;

  let sort: t => Sort.t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
  let equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = any_t;

  let sort = (any: t): Sort.t =>
    switch (any) {
    | Exp(_) => Exp
    | Pat(_) => Pat
    | Typ(_) => Typ
    | TPat(_) => TPat
    | Rul(_) => Rul
    | Any(_) => Any
    };

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x: any_t,
      ) => {
    let rec_call = (y: any_t): any_t =>
      switch (y) {
      | Exp(x) =>
        Exp(Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, x))
      | Pat(x) =>
        Pat(Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, x))
      | Typ(x) =>
        Typ(Typ.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, x))
      | TPat(x) =>
        TPat(
          TPat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, x),
        )
      | Rul(x) =>
        Rul(Rul.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, x))
      | Any () => Any()
      };
    x |> f_any(rec_call);
  };

  let fast_equal = (x: t, y: t) =>
    switch (x, y) {
    | (Exp(x), Exp(y)) => Exp.fast_equal(x, y)
    | (Pat(x), Pat(y)) => Pat.fast_equal(x, y)
    | (Typ(x), Typ(y)) => Typ.fast_equal(x, y)
    | (TPat(x), TPat(y)) => TPat.fast_equal(x, y)
    | (Rul(x), Rul(y)) => Rul.fast_equal(x, y)
    | (Any (), Any ()) => true
    | (Exp(_), _)
    | (Pat(_), _)
    | (Typ(_), _)
    | (TPat(_), _)
    | (Rul(_), _)
    | (Any (), _) => false
    };

  let equal = fast_equal;
}
and Exp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = exp_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = exp_t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type deferral_position = deferral_position_t;
  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;

  let fast_equal: (~ignore_constructor_types: bool=?, t, t) => bool;
  let equal: (~ignore_constructor_types: bool=?, t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = exp_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = exp_t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type deferral_position = deferral_position_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x,
      ) => {
    let exp_map_term =
      Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let pat_map_term =
      Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let typ_map_term =
      Typ.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let tpat_map_term =
      TPat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let flt_map_term =
      StepperFilterKind.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_any,
      );
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_)
        | Atom(_)
        | Constructor(_)
        | Label(_)
        | Deferral(_)
        | Var(_)
        | LivelitName(_)
        | Undefined => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | DynamicErrorHole(e, err) => DynamicErrorHole(exp_map_term(e), err)
        | ListLit(ts) => ListLit(List.map(exp_map_term, ts))
        | Fun(p, e, t, f) =>
          Fun(
            pat_map_term(p),
            exp_map_term(e),
            Option.map(typ_map_term, t),
            f,
          )
        | TypFun(tp, e, f) => TypFun(tpat_map_term(tp), exp_map_term(e), f)
        | TupLabel(label, e) =>
          TupLabel(exp_map_term(label), exp_map_term(e))
        | Tuple(xs) => Tuple(List.map(exp_map_term, xs))
        | TupleExtension(e1, e2) =>
          TupleExtension(exp_map_term(e1), exp_map_term(e2))
        | Dot(e1, e2) => Dot(exp_map_term(e1), exp_map_term(e2))
        | Let(p, e1, e2) =>
          Let(pat_map_term(p), exp_map_term(e1), exp_map_term(e2))
        | Theorem(p, e1, e2) =>
          Theorem(pat_map_term(p), exp_map_term(e1), exp_map_term(e2))
        | ProofObject(t) => ProofObject(exp_map_term(t))
        | Forall(p, e) => Forall(pat_map_term(p), exp_map_term(e))
        | FixF(p, e, env) => FixF(pat_map_term(p), exp_map_term(e), env)
        | TyAlias(tp, t, e) =>
          TyAlias(tpat_map_term(tp), typ_map_term(t), exp_map_term(e))
        | Use(t, e) => Use(typ_map_term(t), exp_map_term(e))
        | Ap(op, e1, e2) => Ap(op, exp_map_term(e1), exp_map_term(e2))
        | TypAp(e, t) => TypAp(exp_map_term(e), typ_map_term(t))
        | DeferredAp(e, es) =>
          DeferredAp(exp_map_term(e), List.map(exp_map_term, es))
        | If(e1, e2, e3) =>
          If(exp_map_term(e1), exp_map_term(e2), exp_map_term(e3))
        | Seq(e1, e2) => Seq(exp_map_term(e1), exp_map_term(e2))
        | Test(e) => Test(exp_map_term(e))
        | HintedTest(e, h) => HintedTest(exp_map_term(e), exp_map_term(h))
        | Filter(f, e) => Filter(flt_map_term(f), exp_map_term(e))
        | Closure(env, e) => Closure(env, exp_map_term(e))
        | Parens(e) => Parens(exp_map_term(e))
        | Probe(e, tag) => Probe(exp_map_term(e), tag)
        | Cons(e1, e2) => Cons(exp_map_term(e1), exp_map_term(e2))
        | ListConcat(e1, e2) =>
          ListConcat(exp_map_term(e1), exp_map_term(e2))
        | UnOp(op, e) => UnOp(op, exp_map_term(e))
        | BinOp(op, e1, e2) =>
          BinOp(op, exp_map_term(e1), exp_map_term(e2))
        | BuiltinFun(str) => BuiltinFun(str)
        | Match(e, rls) =>
          Match(
            exp_map_term(e),
            List.map(
              ((p, e)) => (pat_map_term(p), exp_map_term(e)),
              rls,
            ),
          )
        | Asc(e, t) => Asc(exp_map_term(e), typ_map_term(t))
        },
    };
    x |> f_exp(rec_call);
  };

  let rec fast_equal = (~ignore_constructor_types: bool=false, e1: t, e2: t) => {
    let fast_equal = fast_equal(~ignore_constructor_types);
    switch (e1 |> Grammar.Annotated.term_of, e2 |> Grammar.Annotated.term_of) {
    | (DynamicErrorHole(x, _), _)
    | (Parens(x), _) => fast_equal(x, e2)
    | (_, DynamicErrorHole(x, _))
    | (_, Parens(x)) => fast_equal(e1, x)
    /* Hack to make EvalResult.calculate recalc after adding a probe.
     * We should clarify syntactic/semantic equality here,
     * See https://github.com/hazelgrove/hazel/issues/1563 */
    | (Probe(x1, _), Probe(x2, _)) => fast_equal(x1, x2)
    | (Probe(_, _), _) => false
    | (EmptyHole, EmptyHole) => true
    | (Undefined, Undefined) => true
    | (Invalid(s1), Invalid(s2)) => s1 == s2
    | (MultiHole(xs), MultiHole(ys)) when List.length(xs) == List.length(ys) =>
      List.equal(Any.fast_equal, xs, ys)
    | (Deferral(d1), Deferral(d2)) => d1 == d2
    | (Atom(c1), Atom(c2)) => c1 == c2
    | (Label(l1), Label(l2)) => l1 == l2
    | (ListLit(xs), ListLit(ys)) =>
      List.length(xs) == List.length(ys) && List.equal(fast_equal, xs, ys)
    | (Constructor(c1, _), Constructor(c2, _))
        when ignore_constructor_types == true =>
      c1 == c2
    | (Constructor(c1, Some(Some(ty1))), Constructor(c2, Some(Some(ty2)))) =>
      c1 == c2 && Typ.fast_equal(ty1, ty2)
    | (Constructor(c1, Some(None)), Constructor(c2, Some(None)))
    | (Constructor(c1, None), Constructor(c2, None)) => c1 == c2
    | (Fun(p1, e1, t1, _), Fun(p2, e2, t2, _)) =>
      Pat.fast_equal(p1, p2)
      && fast_equal(e1, e2)
      && Option.equal(Typ.fast_equal, t1, t2)
    | (TypFun(tp1, e1, _), TypFun(tp2, e2, _)) =>
      TPat.fast_equal(tp1, tp2) && fast_equal(e1, e2)
    | (Tuple(xs), Tuple(ys)) =>
      List.length(xs) == List.length(ys) && List.equal(fast_equal, xs, ys)
    | (Var(v1), Var(v2)) => v1 == v2
    | (Let(p1, e1, e2), Let(p2, e3, e4)) =>
      Pat.fast_equal(p1, p2) && fast_equal(e1, e3) && fast_equal(e2, e4)
    | (Theorem(p1, e1, e2), Theorem(p2, e3, e4)) =>
      Pat.fast_equal(p1, p2) && fast_equal(e1, e3) && fast_equal(e2, e4)
    | (ProofObject(t1), ProofObject(t2)) => Exp.fast_equal(t1, t2)
    | (Forall(p1, e1), Forall(p2, e2)) =>
      Pat.fast_equal(p1, p2) && fast_equal(e1, e2)
    | (FixF(p1, e1, c1), FixF(p2, e2, c2)) =>
      Pat.fast_equal(p1, p2)
      && fast_equal(e1, e2)
      && Option.equal(ClosureEnvironment.id_equal, c1, c2)
    | (TyAlias(tp1, t1, e1), TyAlias(tp2, t2, e2)) =>
      TPat.fast_equal(tp1, tp2)
      && Typ.fast_equal(t1, t2)
      && fast_equal(e1, e2)
    | (Use(t1, e1), Use(t2, e2)) =>
      Typ.fast_equal(t1, t2) && fast_equal(e1, e2)
    | (Ap(d1, e1, e2), Ap(d2, e3, e4)) =>
      d1 == d2 && fast_equal(e1, e3) && fast_equal(e2, e4)
    | (TypAp(e1, t1), TypAp(e2, t2)) =>
      fast_equal(e1, e2) && Typ.fast_equal(t1, t2)
    | (DeferredAp(e1, es1), DeferredAp(e2, es2)) =>
      List.length(es1) == List.length(es2)
      && fast_equal(e1, e2)
      && List.equal(fast_equal, es1, es2)
    | (If(e1, e2, e3), If(e4, e5, e6)) =>
      fast_equal(e1, e4) && fast_equal(e2, e5) && fast_equal(e3, e6)
    | (Seq(e1, e2), Seq(e3, e4)) =>
      fast_equal(e1, e3) && fast_equal(e2, e4)
    | (Test(e1), Test(e2)) => fast_equal(e1, e2)
    | (HintedTest(e1, e2), HintedTest(e3, e4)) =>
      fast_equal(e1, e3) && fast_equal(e2, e4)
    | (Filter(f1, e1), Filter(f2, e2)) =>
      StepperFilterKind.fast_equal(f1, f2) && fast_equal(e1, e2)
    | (Closure(c1, e1), Closure(c2, e2)) =>
      ClosureEnvironment.id_equal(c1, c2) && fast_equal(e1, e2)
    | (Cons(e1, e2), Cons(e3, e4)) =>
      fast_equal(e1, e3) && fast_equal(e2, e4)
    | (LivelitName(s1), LivelitName(s2)) => s1 == s2
    | (ListConcat(e1, e2), ListConcat(e3, e4)) =>
      fast_equal(e1, e3) && fast_equal(e2, e4)
    | (UnOp(o1, e1), UnOp(o2, e2)) => o1 == o2 && fast_equal(e1, e2)
    | (BinOp(o1, e1, e2), BinOp(o2, e3, e4)) =>
      o1 == o2 && fast_equal(e1, e3) && fast_equal(e2, e4)
    | (BuiltinFun(f1), BuiltinFun(f2)) => f1 == f2
    | (Match(e1, rls1), Match(e2, rls2)) =>
      fast_equal(e1, e2)
      && List.length(rls1) == List.length(rls2)
      && List.for_all2(
           ((p1, e1), (p2, e2)) =>
             Pat.fast_equal(p1, p2) && fast_equal(e1, e2),
           rls1,
           rls2,
         )
    | (Asc(e1, t1), Asc(e2, t2)) =>
      fast_equal(e1, e2) && Typ.fast_equal(t1, t2)
    | (TupLabel(e1, e2), TupLabel(e3, e4)) =>
      fast_equal(e1, e3) && fast_equal(e2, e4)
    | (Dot(e1, e2), Dot(e3, e4)) =>
      fast_equal(e1, e3) && fast_equal(e2, e4)
    | (TupleExtension(e1, e2), TupleExtension(e3, e4)) =>
      fast_equal(e1, e3) && fast_equal(e2, e4)
    | (Invalid(_), _)
    | (Deferral(_), _)
    | (Atom(_), _)
    | (Label(_), _)
    | (LivelitName(_), _)
    | (ListLit(_), _)
    | (Constructor(_), _)
    | (Fun(_), _)
    | (TypFun(_), _)
    | (Tuple(_), _)
    | (TupLabel(_), _)
    | (TupleExtension(_), _)
    | (Dot(_), _)
    | (Var(_), _)
    | (Let(_), _)
    | (Theorem(_), _)
    | (ProofObject(_), _)
    | (Forall(_), _)
    | (FixF(_), _)
    | (TyAlias(_), _)
    | (Use(_), _)
    | (Ap(_), _)
    | (TypAp(_), _)
    | (DeferredAp(_), _)
    | (If(_), _)
    | (Seq(_), _)
    | (Test(_), _)
    | (HintedTest(_, _), _)
    | (Filter(_), _)
    | (Closure(_), _)
    | (Cons(_), _)
    | (ListConcat(_), _)
    | (UnOp(_), _)
    | (BinOp(_), _)
    | (BuiltinFun(_), _)
    | (Match(_), _)
    | (Asc(_), _)
    | (MultiHole(_), _)
    | (EmptyHole, _)
    | (Undefined, _) => false
    };
  };
  let equal = fast_equal;
}
and Pat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = pat_term; // The second Typ.t field is only meaningful in dynamic patterns
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = pat_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
  let equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = pat_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = pat_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x,
      ) => {
    let pat_map_term =
      Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let typ_map_term =
      Typ.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_)
        | Wild
        | Atom(_)
        | Constructor(_)
        | Label(_)
        | Var(_) => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | ListLit(ts) => ListLit(List.map(pat_map_term, ts))
        | Ap(e1, e2) => Ap(pat_map_term(e1), pat_map_term(e2))
        | Cons(e1, e2) => Cons(pat_map_term(e1), pat_map_term(e2))
        | Tuple(xs) => Tuple(List.map(pat_map_term, xs))
        | TupLabel(label, e) =>
          TupLabel(pat_map_term(label), pat_map_term(e))
        | Parens(e) => Parens(pat_map_term(e))
        | Probe(e, tag) => Probe(pat_map_term(e), tag)
        | Asc(e, t) => Asc(pat_map_term(e), typ_map_term(t))
        },
    };
    x |> f_pat(rec_call);
  };

  let rec fast_equal = (p1: t, p2: t) =>
    switch (p1 |> Grammar.Annotated.term_of, p2 |> Grammar.Annotated.term_of) {
    /* Below is kind of a hack to make EvalResult.calculate go after adding a projector.
     * We should clarify syntactic/semantic equality here */
    | (Probe(x1, _), Probe(x2, _)) => fast_equal(x1, x2)
    | (Probe(_, _), _) => false
    | (Parens(x), _) => fast_equal(x, p2)
    | (_, Parens(x)) => fast_equal(p1, x)
    | (EmptyHole, EmptyHole) => true
    | (MultiHole(xs), MultiHole(ys)) =>
      List.length(xs) == List.length(ys)
      && List.equal(Any.fast_equal, xs, ys)
    | (Invalid(s1), Invalid(s2)) => s1 == s2
    | (Wild, Wild) => true
    | (Atom(c1), Atom(c2)) => c1 == c2
    | (Label(s1), Label(s2)) => s1 == s2
    | (Constructor(c1, Some(Some(t1))), Constructor(c2, Some(Some(t2)))) =>
      c1 == c2 && Typ.fast_equal(t1, t2)
    | (Constructor(c1, Some(None)), Constructor(c2, Some(None)))
    | (Constructor(c1, None), Constructor(c2, None)) => c1 == c2
    | (Var(v1), Var(v2)) => v1 == v2
    | (ListLit(xs), ListLit(ys)) =>
      List.length(xs) == List.length(ys) && List.equal(fast_equal, xs, ys)
    | (Cons(x1, y1), Cons(x2, y2)) =>
      fast_equal(x1, x2) && fast_equal(y1, y2)
    | (TupLabel(label1, d1'), TupLabel(label2, d2')) =>
      fast_equal(label1, label2) && fast_equal(d1', d2')
    | (Tuple(xs), Tuple(ys)) =>
      List.length(xs) == List.length(ys) && List.equal(fast_equal, xs, ys)
    | (Ap(x1, y1), Ap(x2, y2)) => fast_equal(x1, x2) && fast_equal(y1, y2)
    | (Asc(x1, t1), Asc(x2, u1)) =>
      fast_equal(x1, x2) && Typ.fast_equal(t1, u1)
    | (EmptyHole, _)
    | (MultiHole(_), _)
    | (Invalid(_), _)
    | (Wild, _)
    | (Atom(_), _)
    | (Label(_), _)
    | (ListLit(_), _)
    | (Constructor(_), _)
    | (Cons(_), _)
    | (Var(_), _)
    | (TupLabel(_), _)
    | (Tuple(_), _)
    | (Ap(_), _)
    | (Asc(_), _) => false
    };
  let equal = fast_equal;
}
and Typ: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = typ_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = typ_t;

  type sum_map = ConstructorMap.t(t);

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;

  let subst: (t, TPat.t, t) => t;

  let fast_equal: (~alpha_equivalence: bool=?, t, t) => bool;
  let equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = typ_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = typ_t;

  type sum_map = ConstructorMap.t(t);

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x,
      ) => {
    let typ_map_term =
      Typ.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let tpat_map_term =
      TPat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let exp_map_term =
      Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Unknown(Hole(EmptyHole))
        | Unknown(Hole(Invalid(_)))
        | Unknown(SynSwitch)
        | Unknown(Internal)
        | Atom(_)
        | Label(_)
        | Var(_) => term
        | List(t) => List(typ_map_term(t))
        | Unknown(Hole(MultiHole(things))) =>
          Unknown(Hole(MultiHole(List.map(any_map_term, things))))
        | Prod(xs) => Prod(List.map(typ_map_term, xs))
        | TupLabel(label, e) =>
          TupLabel(typ_map_term(label), typ_map_term(e))
        | Parens(e) => Parens(typ_map_term(e))
        | Arrow(t1, t2) => Arrow(typ_map_term(t1), typ_map_term(t2))
        | Sum(variants) =>
          Sum(
            List.map(
              fun
              | ConstructorMap.Variant(c, ids, t) =>
                ConstructorMap.Variant(c, ids, Option.map(typ_map_term, t))
              | ConstructorMap.BadEntry(t) =>
                ConstructorMap.BadEntry(typ_map_term(t)),
              variants,
            ),
          )
        | Rec(tp, t) => Rec(tpat_map_term(tp), typ_map_term(t))
        | Poly(tp, t) => Poly(tpat_map_term(tp), typ_map_term(t))
        | ProofOf(e) => ProofOf(exp_map_term(e))
        },
    };
    x |> f_typ(rec_call);
  };

  let rec subst = (s: t, x: TPat.t, ty: t): typ_t => {
    switch (TPat.tyvar_of_utpat(x)) {
    | Some(str) =>
      let (term, rewrap) = Grammar.Annotated.unwrap(ty);
      switch (term) {
      | Atom(_) => ty
      | Label(name) => Grammar.Label(name) |> rewrap
      | Unknown(prov) => Unknown(prov) |> rewrap
      | Arrow(ty1, ty2) =>
        Arrow(subst(s, x, ty1), subst(s, x, ty2)) |> rewrap
      | Prod(tys) => Prod(List.map(subst(s, x), tys)) |> rewrap
      | TupLabel(label, ty) => TupLabel(label, subst(s, x, ty)) |> rewrap
      | Sum(sm) =>
        Sum(ConstructorMap.map(Option.map(subst(s, x)), sm)) |> rewrap
      | Poly(tp2, ty)
          when TPat.tyvar_of_utpat(x) == TPat.tyvar_of_utpat(tp2) =>
        Poly(tp2, ty) |> rewrap
      | Poly(tp2, ty) => Poly(tp2, subst(s, x, ty)) |> rewrap
      | Rec(tp2, ty) when TPat.tyvar_of_utpat(x) == TPat.tyvar_of_utpat(tp2) =>
        Rec(tp2, ty) |> rewrap
      | Rec(tp2, ty) => Rec(tp2, subst(s, x, ty)) |> rewrap
      | ProofOf(e) => ProofOf(e) |> rewrap // TODO[Matt]: do we need to substitute into the expression?
      | List(ty) => List(subst(s, x, ty)) |> rewrap
      | Var(y) => str == y ? s : Var(y) |> rewrap
      | Parens(ty) => Parens(subst(s, x, ty)) |> rewrap
      };
    | None => ty
    };
  };

  /* Type Equality: This coincides with alpha equivalence for normalized types.
     Other types may be equivalent but this will not detect so if they are not normalized. */

  let rec eq_internal = (~alpha_equivalence: bool, n: int, t1: t, t2: t) => {
    switch (Grammar.Annotated.term_of(t1), Grammar.Annotated.term_of(t2)) {
    | (Parens(t1), _) => eq_internal(~alpha_equivalence, n, t1, t2)
    | (_, Parens(t2)) => eq_internal(~alpha_equivalence, n, t1, t2)
    | (TupLabel(label1, t1'), TupLabel(label2, t2')) =>
      eq_internal(~alpha_equivalence, n, label1, label2)
      && eq_internal(~alpha_equivalence, n, t1', t2')
    | (TupLabel(_), _) => false
    | (Rec(x1, t1), Rec(x2, t2))
    | (Poly(x1, t1), Poly(x2, t2)) =>
      if (alpha_equivalence) {
        let alpha_subst =
          subst({
            term: Var("=" ++ string_of_int(n)),
            annotation: {
              ids: [Id.invalid],
            },
          });
        eq_internal(
          ~alpha_equivalence,
          n + 1,
          alpha_subst(x1, t1),
          alpha_subst(x2, t2),
        );
      } else {
        TPat.fast_equal(x1, x2)
        && eq_internal(~alpha_equivalence, n + 1, t1, t2);
      }
    | (Rec(_), _) => false
    | (Poly(_), _) => false
    | (ProofOf(e1), ProofOf(e2)) => Exp.fast_equal(e1, e2)
    | (ProofOf(_), _) => false
    | (Atom(name1), Atom(name2)) => name1 == name2
    | (Atom(_), _) => false
    | (Label(name1), Label(name2)) =>
      LabeledTuple.match_labels(name1, name2)
    | (Label(_), _) => false
    | (Unknown(_), Unknown(_)) => true
    | (Unknown(_), _) => false
    | (Arrow(t1, t2), Arrow(t1', t2')) =>
      eq_internal(~alpha_equivalence, n, t1, t1')
      && eq_internal(~alpha_equivalence, n, t2, t2')
    | (Arrow(_), _) => false
    | (Prod(tys1), Prod(tys2)) =>
      List.equal(eq_internal(~alpha_equivalence, n), tys1, tys2)
    | (Prod(_), _) => false
    | (List(t1), List(t2)) => eq_internal(~alpha_equivalence, n, t1, t2)
    | (List(_), _) => false
    | (Sum(sm1), Sum(sm2)) =>
      /* Does not normalize the types. */
      ConstructorMap.equal(eq_internal(~alpha_equivalence, n), sm1, sm2)
    | (Sum(_), _) => false
    | (Var(n1), Var(n2)) => n1 == n2
    | (Var(_), _) => false
    };
  };

  let fast_equal = (~alpha_equivalence=true, t1, t2) =>
    eq_internal(~alpha_equivalence, 0, t1, t2);
  let equal: (t, t) => bool = fast_equal(~alpha_equivalence=true);
}
and TPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = tpat_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = tpat_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;

  let tyvar_of_utpat: t => option(string);

  let fast_equal: (t, t) => bool;
  let equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = tpat_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = tpat_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x,
      ) => {
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_)
        | Var(_) => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        },
    };
    x |> f_tpat(rec_call);
  };

  let tyvar_of_utpat = ({term, _}: t) =>
    switch (term) {
    | Var(x) => Some(x)
    | _ => None
    };

  let fast_equal = (tp1: t, tp2: t) =>
    switch (
      tp1 |> Grammar.Annotated.term_of,
      tp2 |> Grammar.Annotated.term_of,
    ) {
    | (EmptyHole, EmptyHole) => true
    | (Invalid(s1), Invalid(s2)) => s1 == s2
    | (MultiHole(xs), MultiHole(ys)) =>
      List.length(xs) == List.length(ys)
      && List.equal(Any.fast_equal, xs, ys)
    | (Var(x), Var(y)) => x == y
    | (EmptyHole, _)
    | (Invalid(_), _)
    | (MultiHole(_), _)
    | (Var(_), _) => false
    };
  let equal = fast_equal;
}
and Rul: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = rul_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = rul_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
  let equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = rul_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = rul_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x,
      ) => {
    let exp_map_term =
      Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let pat_map_term =
      Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Invalid(_) => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | Rules(e, rls) =>
          Rules(
            exp_map_term(e),
            List.map(
              ((p, e)) => (pat_map_term(p), exp_map_term(e)),
              rls,
            ),
          )
        },
    };
    x |> f_rul(rec_call);
  };

  let fast_equal = (r1: t, r2: t) =>
    switch (r1 |> Grammar.Annotated.term_of, r2 |> Grammar.Annotated.term_of) {
    | (Invalid(s1), Invalid(s2)) => s1 == s2
    | (MultiHole(xs), MultiHole(ys)) =>
      List.length(xs) == List.length(ys)
      && List.equal(Any.fast_equal, xs, ys)
    | (Rules(e1, rls1), Rules(e2, rls2)) =>
      Exp.fast_equal(e1, e2)
      && List.length(rls1) == List.length(rls2)
      && List.for_all2(
           ((p1, e1), (p2, e2)) =>
             Pat.fast_equal(p1, p2) && Exp.fast_equal(e1, e2),
           rls1,
           rls2,
         )
    | (Invalid(_), _)
    | (MultiHole(_), _)
    | (Rules(_), _) => false
    };
  let equal = fast_equal;
}

and Environment: {
  include
     (module type of VarBstMap.Ordered) with
      type t_('a) = VarBstMap.Ordered.t_('a);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = environment_t;
  let pp: (Format.formatter, t) => unit;
} = {
  include VarBstMap.Ordered;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = environment_t;

  [@deriving show({with_path: false})]
  type entries = list((Var.t, Exp.t));

  let pp = (f, map: t) => pp_entries(f, VarBstMap.Ordered.to_listo(map));
}

and ClosureEnvironment: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = closure_environment_t;

  let empty: t;

  let of_environment: Environment.t => t;

  let map_of: t => Environment.t;
  let call_stack_of: t => Probe.call_stack;

  let id_equal: (closure_environment_t, closure_environment_t) => bool;

  let lookup: (t, Var.t) => option(Exp.t);
  let update_env: (Environment.t => Environment.t, t) => t;
  let extend_eval:
    (~ap_id: Id.t=?, ~call_stack: Probe.call_stack, Environment.t, t) => t;

  let to_list: t => list((Var.t, Exp.t));
} = {
  module Inner: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = closure_environment_t;

    let wrap: (Id.t, Environment.t, Probe.call_stack) => t;

    let id_of: t => Id.t;
    let map_of: t => Environment.t;
    let call_stack_of: t => Probe.call_stack;
  } = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = closure_environment_t;

    let wrap = (id, env, call_stack): t => {
      id,
      env,
      call_stack,
    };

    let id_of = (t: t) => t.id;
    let map_of = (t: t) => t.env;
    let call_stack_of = (t: t) => t.call_stack;
  };
  include Inner;

  let to_list = env => env |> map_of |> Environment.to_listo;

  let of_environment = env => wrap(Id.mk(), env, []);

  /* Equals only needs to check environment id's (faster than structural equality
   * checking.) */
  let id_equal = (env1, env2) => id_of(env1) == id_of(env2);

  let empty = Environment.empty |> of_environment;

  let lookup = (env, x) =>
    env |> map_of |> (map => Environment.lookup(map, x));

  let update_env = (f, env) => env |> map_of |> f |> of_environment;

  /* Extend the environment with new bindings. ~ap_id is an optional argument which
   * will add an entry in a stack of function application syntax ids, used to
   * represent and track the call stack for use by live value probes. */
  let extend_eval =
      (
        ~ap_id: option(Id.t)=?,
        ~call_stack: Probe.call_stack,
        new_bindings: Environment.t,
        env_to_extend: t,
      )
      : t => {
    {
      id: Id.mk(),
      env: Environment.union(new_bindings, map_of(env_to_extend)),
      call_stack: Option.to_list(ap_id) @ call_stack,
    };
  };
}
and StepperFilterKind: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = stepper_filter_kind_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;

  let map: (Exp.t => Exp.t, t) => t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = stepper_filter_kind_t;

  let map = (mapper, filter: t): t => {
    switch (filter) {
    | Filter({act, pat}) =>
      Filter({
        act,
        pat: mapper(pat),
      })
    | Residue(idx, act) => Residue(idx, act)
    };
  };

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
      ) => {
    let exp_map_term =
      Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    (
      fun
      | Filter({pat: e, act}) =>
        Filter({
          pat: exp_map_term(e),
          act,
        })
      | Residue(i, a) => Residue(i, a):
        t => t
    );
  };

  let fast_equal = (f1: t, f2: t) =>
    switch (f1, f2) {
    | (Filter({pat: e1, act: a1}), Filter({pat: e2, act: a2})) =>
      Exp.fast_equal(e1, e2) && a1 == a2
    | (Residue(i1, a1), Residue(i2, a2)) => i1 == i2 && a1 == a2
    | (Filter(_), _)
    | (Residue(_), _) => false
    };
};
