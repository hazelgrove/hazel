open Util;

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
       ~f_exp: (Exp.t('a) => Exp.t('a) , Exp.t('a) ) => Exp.t('a) =?,
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

module rec Any: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('a) =
    | Exp(Exp.t('a))
    | Pat(Pat.t('a))
    | Typ(Typ.t('a))
    | TPat(TPat.t('a))
    | Rul(Rul.t('a))
    | Nul(unit)
    | Any(unit);

  let map_term:
    (
      ~f_exp: (Exp.t('a) => Exp.t('a), Exp.t('a)) => Exp.t('a)=?,
      ~f_pat: (Pat.t('a) => Pat.t('a), Pat.t('a)) => Pat.t('a)=?,
      ~f_typ: (Typ.t('a) => Typ.t('a), Typ.t('a)) => Typ.t('a)=?,
      ~f_tpat: (TPat.t('a) => TPat.t('a), TPat.t('a)) => TPat.t('a)=?,
      ~f_rul: (Rul.t('a) => Rul.t('a), Rul.t('a)) => Rul.t('a)=?,
      ~f_any: (Any.t('a) => Any.t('a), Any.t('a)) => Any.t('a)=?,
      t('a)
    ) =>
    t('a);

  let fast_equal: (t('a), t('a)) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('a) =
    | Exp(Exp.t('a))
    | Pat(Pat.t('a))
    | Typ(Typ.t('a))
    | TPat(TPat.t('a))
    | Rul(Rul.t('a))
    | Nul(unit)
    | Any(unit);

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
    let rec_call = y =>
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
      | Nul () => Nul()
      | Any () => Any()
      };
    x |> f_any(rec_call);
  };

  let fast_equal = (x, y) =>
    switch (x, y) {
    | (Exp(x), Exp(y)) => Exp.fast_equal(x, y)
    | (Pat(x), Pat(y)) => Pat.fast_equal(x, y)
    | (Typ(x), Typ(y)) => Typ.fast_equal(x, y)
    | (TPat(x), TPat(y)) => TPat.fast_equal(x, y)
    | (Rul(x), Rul(y)) => Rul.fast_equal(x, y)
    | (Nul (), Nul ()) => true
    | (Any (), Any ()) => true
    | (Exp(_), _)
    | (Pat(_), _)
    | (Typ(_), _)
    | (TPat(_), _)
    | (Rul(_), _)
    | (Nul (), _)
    | (Any (), _) => false
    };
}
and Exp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type deferral_position =
    | InAp
    | OutsideAp;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term('a) =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t('a)))
    | DynamicErrorHole(t('a), InvalidOperationError.t)
    | FailedCast(t('a), Typ.t('a), Typ.t('a))
    | Deferral(deferral_position)
    | Undefined
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(list(t('a)))
    | Constructor(string, Typ.t('a)) // Typ.t field is only meaningful in dynamic expressions
    | Fun(
        Pat.t('a),
        t('a),
        [@show.opaque] option(ClosureEnvironment.t),
        option(Var.t),
      )
    | TypFun(TPat.t('a), t('a), option(Var.t))
    | Tuple(list(t('a)))
    | Var(Var.t)
    | Let(Pat.t('a), t('a), t('a))
    | FixF(Pat.t('a), t('a), option(ClosureEnvironment.t))
    | TyAlias(TPat.t('a), Typ.t('a), t('a))
    | Ap(Operators.ap_direction, t('a), t('a))
    | TypAp(t('a), Typ.t('a))
    | DeferredAp(t('a), list(t('a)))
    | If(t('a), t('a), t('a))
    | Seq(t('a), t('a))
    | Test(t('a))
    | Filter(StepperFilterKind.t('a), t('a))
    | Closure([@show.opaque] ClosureEnvironment.t, t('a))
    | Parens(t('a)) // (
    | Cons(t('a), t('a))
    | ListConcat(t('a), t('a))
    | UnOp(Operators.op_un, t('a))
    | BinOp(Operators.op_bin, t('a), t('a))
    | BuiltinFun(string)
    | Match(t('a), list((Pat.t('a), t('a))))
    /* INVARIANT: in dynamic expressions, casts must be between
       two consistent types. Both types should be normalized in
       dynamics for the cast calculus to work right. */
    | Cast(t('a), Typ.t('a), Typ.t('a)) // first Typ.t field is only meaningful in dynamic expressions
  and t('a) = Annotated.t(term('a), 'a);

  let map_term:
    (
      ~f_exp: (Exp.t('a) => Exp.t('a), Exp.t('a)) => Exp.t('a)=?,
      ~f_pat: (Pat.t('a) => Pat.t('a), Pat.t('a)) => Pat.t('a)=?,
      ~f_typ: (Typ.t('a) => Typ.t('a), Typ.t('a)) => Typ.t('a)=?,
      ~f_tpat: (TPat.t('a) => TPat.t('a), TPat.t('a)) => TPat.t('a)=?,
      ~f_rul: (Rul.t('a) => Rul.t('a), Rul.t('a)) => Rul.t('a)=?,
      ~f_any: (Any.t('a) => Any.t('a), Any.t('a)) => Any.t('a)=?,
      t('a)
    ) =>
    t('a);

  let fast_equal: (t('a), t('a)) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type deferral_position =
    | InAp
    | OutsideAp;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term('a) =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t('a)))
    | DynamicErrorHole(t('a), InvalidOperationError.t)
    | FailedCast(t('a), Typ.t('a), Typ.t('a))
    | Deferral(deferral_position)
    | Undefined
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | ListLit(list(t('a)))
    | Constructor(string, Typ.t('a)) // Typ.t field is only meaningful in dynamic expressions
    | Fun(
        Pat.t('a),
        t('a),
        [@show.opaque] option(ClosureEnvironment.t),
        option(Var.t),
      )
    | TypFun(TPat.t('a), t('a), option(Var.t))
    | Tuple(list(t('a)))
    | Var(Var.t)
    | Let(Pat.t('a), t('a), t('a))
    | FixF(Pat.t('a), t('a), option(ClosureEnvironment.t))
    | TyAlias(TPat.t('a), Typ.t('a), t('a))
    | Ap(Operators.ap_direction, t('a), t('a))
    | TypAp(t('a), Typ.t('a))
    | DeferredAp(t('a), list(t('a)))
    | If(t('a), t('a), t('a))
    | Seq(t('a), t('a))
    | Test(t('a))
    | Filter(StepperFilterKind.t('a), t('a))
    | Closure([@show.opaque] ClosureEnvironment.t, t('a))
    | Parens(t('a)) // (
    | Cons(t('a), t('a))
    | ListConcat(t('a), t('a))
    | UnOp(Operators.op_un, t('a))
    | BinOp(Operators.op_bin, t('a), t('a))
    | BuiltinFun(string)
    | Match(t('a), list((Pat.t('a), t('a))))
    /* INVARIANT: in dynamic expressions, casts must be between
       two consistent types. Both types should be normalized in
       dynamics for the cast calculus to work right. */
    | Cast(t('a), Typ.t('a), Typ.t('a)) // first Typ.t field is only meaningful in dynamic expressions
  and t('a) = Annotated.t(term('a), 'a);

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
    let rec_call = ({term, _} as exp: t('a)) => {
      ...exp,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_)
        | Bool(_)
        | Int(_)
        | Float(_)
        | Constructor(_)
        | String(_)
        | Deferral(_)
        | Var(_)
        | Undefined => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | DynamicErrorHole(e, err: InvalidOperationError.t) =>
          let foo = exp_map_term(e);
          DynamicErrorHole(foo, err);
        | FailedCast(e, t1, t2) => FailedCast(exp_map_term(e), t1, t2)
        | ListLit(ts) => ListLit(List.map(exp_map_term, ts))
        | Fun(p, e, env, f) =>
          Fun(pat_map_term(p), exp_map_term(e), env, f)
        | TypFun(tp, e, f) => TypFun(tpat_map_term(tp), exp_map_term(e), f)
        | Tuple(xs) => Tuple(List.map(exp_map_term, xs))
        | Let(p, e1, e2) =>
          Let(pat_map_term(p), exp_map_term(e1), exp_map_term(e2))
        | FixF(p, e, env) => FixF(pat_map_term(p), exp_map_term(e), env)
        | TyAlias(tp, t, e) =>
          TyAlias(tpat_map_term(tp), typ_map_term(t), exp_map_term(e))
        | Ap(op, e1, e2) => Ap(op, exp_map_term(e1), exp_map_term(e2))
        | TypAp(e, t) => TypAp(exp_map_term(e), typ_map_term(t))
        | DeferredAp(e, es) =>
          DeferredAp(exp_map_term(e), List.map(exp_map_term, es))
        | If(e1, e2, e3) =>
          If(exp_map_term(e1), exp_map_term(e2), exp_map_term(e3))
        | Seq(e1, e2) => Seq(exp_map_term(e1), exp_map_term(e2))
        | Test(e) => Test(exp_map_term(e))
        | Filter(f, e) => Filter(flt_map_term(f), exp_map_term(e))
        | Closure(env, e) => Closure(env, exp_map_term(e))
        | Parens(e) => Parens(exp_map_term(e))
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
        | Cast(e, t1, t2) => Cast(exp_map_term(e), t1, t2)
        },
    };
    x |> f_exp(rec_call);
  };

  let rec fast_equal = (e1: t('a), e2: t('a)) =>
    switch (e1.term, e2.term) {
    | (DynamicErrorHole(x, _), _)
    | (Parens(x), _) => fast_equal(x, e2)
    | (_, DynamicErrorHole(x, _))
    | (_, Parens(x)) => fast_equal(e1, x)
    | (EmptyHole, EmptyHole) => true
    | (Undefined, Undefined) => true
    | (Invalid(s1), Invalid(s2)) => s1 == s2
    | (MultiHole(xs), MultiHole(ys)) when List.length(xs) == List.length(ys) =>
      List.equal(Any.fast_equal, xs, ys)
    | (FailedCast(e1, t1, t2), FailedCast(e2, t3, t4)) =>
      Exp.fast_equal(e1, e2)
      && Typ.fast_equal(t1, t3)
      && Typ.fast_equal(t2, t4)
    | (Deferral(d1), Deferral(d2)) => d1 == d2
    | (Bool(b1), Bool(b2)) => b1 == b2
    | (Int(i1), Int(i2)) => i1 == i2
    | (Float(f1), Float(f2)) => f1 == f2
    | (String(s1), String(s2)) => s1 == s2
    | (ListLit(xs), ListLit(ys)) =>
      List.length(xs) == List.length(ys) && List.equal(fast_equal, xs, ys)
    | (Constructor(c1, ty1), Constructor(c2, ty2)) =>
      c1 == c2 && Typ.fast_equal(ty1, ty2)
    | (Fun(p1, e1, env1, _), Fun(p2, e2, env2, _)) =>
      Pat.fast_equal(p1, p2)
      && fast_equal(e1, e2)
      && Option.equal(ClosureEnvironment.id_equal, env1, env2)
    | (TypFun(tp1, e1, _), TypFun(tp2, e2, _)) =>
      TPat.fast_equal(tp1, tp2) && fast_equal(e1, e2)
    | (Tuple(xs), Tuple(ys)) =>
      List.length(xs) == List.length(ys) && List.equal(fast_equal, xs, ys)
    | (Var(v1), Var(v2)) => v1 == v2
    | (Let(p1, e1, e2), Let(p2, e3, e4)) =>
      Pat.fast_equal(p1, p2) && fast_equal(e1, e3) && fast_equal(e2, e4)
    | (FixF(p1, e1, c1), FixF(p2, e2, c2)) =>
      Pat.fast_equal(p1, p2)
      && fast_equal(e1, e2)
      && Option.equal(ClosureEnvironment.id_equal, c1, c2)
    | (TyAlias(tp1, t1, e1), TyAlias(tp2, t2, e2)) =>
      TPat.fast_equal(tp1, tp2)
      && Typ.fast_equal(t1, t2)
      && fast_equal(e1, e2)
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
    | (Filter(f1, e1), Filter(f2, e2)) =>
      StepperFilterKind.fast_equal(f1, f2) && fast_equal(e1, e2)
    | (Closure(c1, e1), Closure(c2, e2)) =>
      ClosureEnvironment.id_equal(c1, c2) && fast_equal(e1, e2)
    | (Cons(e1, e2), Cons(e3, e4)) =>
      fast_equal(e1, e3) && fast_equal(e2, e4)
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
    | (Cast(e1, t1, t2), Cast(e2, t3, t4)) =>
      fast_equal(e1, e2) && Typ.fast_equal(t1, t3) && Typ.fast_equal(t2, t4)
    | (Invalid(_), _)
    | (FailedCast(_), _)
    | (Deferral(_), _)
    | (Bool(_), _)
    | (Int(_), _)
    | (Float(_), _)
    | (String(_), _)
    | (ListLit(_), _)
    | (Constructor(_), _)
    | (Fun(_), _)
    | (TypFun(_), _)
    | (Tuple(_), _)
    | (Var(_), _)
    | (Let(_), _)
    | (FixF(_), _)
    | (TyAlias(_), _)
    | (Ap(_), _)
    | (TypAp(_), _)
    | (DeferredAp(_), _)
    | (If(_), _)
    | (Seq(_), _)
    | (Test(_), _)
    | (Filter(_), _)
    | (Closure(_), _)
    | (Cons(_), _)
    | (ListConcat(_), _)
    | (UnOp(_), _)
    | (BinOp(_), _)
    | (BuiltinFun(_), _)
    | (Match(_), _)
    | (Cast(_), _)
    | (MultiHole(_), _)
    | (EmptyHole, _)
    | (Undefined, _) => false
    };
}
and Pat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term('a) =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t('a)))
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | String(string)
    | ListLit(list(t('a)))
    | Constructor(string, Typ.t('a)) // Typ.t field is only meaningful in dynamic patterns
    | Cons(t('a), t('a))
    | Var(Var.t)
    | Tuple(list(t('a)))
    | Parens(t('a))
    | Ap(t('a), t('a))
    | Cast(t('a), Typ.t('a), Typ.t('a)) // The second Typ.t field is only meaningful in dynamic patterns
  and t('a) = Annotated.t(term('a), 'a);

  let map_term:
    (
      ~f_exp: (Exp.t('a) => Exp.t('a), Exp.t('a)) => Exp.t('a)=?,
      ~f_pat: (Pat.t('a) => Pat.t('a), Pat.t('a)) => Pat.t('a)=?,
      ~f_typ: (Typ.t('a) => Typ.t('a), Typ.t('a)) => Typ.t('a)=?,
      ~f_tpat: (TPat.t('a) => TPat.t('a), TPat.t('a)) => TPat.t('a)=?,
      ~f_rul: (Rul.t('a) => Rul.t('a), Rul.t('a)) => Rul.t('a)=?,
      ~f_any: (Any.t('a) => Any.t('a), Any.t('a)) => Any.t('a)=?,
      t('a)
    ) =>
    t('a);

  let fast_equal: (t('a), t('a)) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term('a) =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t('a)))
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | String(string)
    | ListLit(list(t('a)))
    | Constructor(string, Typ.t('a))
    | Cons(t('a), t('a))
    | Var(Var.t)
    | Tuple(list(t('a)))
    | Parens(t('a))
    | Ap(t('a), t('a))
    | Cast(t('a), Typ.t('a), Typ.t('a)) // The second one is hidden from the user
  and t('a) = Annotated.t(term('a), 'a);

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
    let rec_call = ({term, _} as exp: t('a)) => {
      ...exp,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_)
        | Wild
        | Bool(_)
        | Int(_)
        | Float(_)
        | Constructor(_)
        | String(_)
        | Var(_) => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | ListLit(ts) => ListLit(List.map(pat_map_term, ts))
        | Ap(e1, e2) => Ap(pat_map_term(e1), pat_map_term(e2))
        | Cons(e1, e2) => Cons(pat_map_term(e1), pat_map_term(e2))
        | Tuple(xs) => Tuple(List.map(pat_map_term, xs))
        | Parens(e) => Parens(pat_map_term(e))
        | Cast(e, t1, t2) =>
          Cast(pat_map_term(e), typ_map_term(t1), typ_map_term(t2))
        },
    };
    x |> f_pat(rec_call);
  };

  let rec fast_equal = (p1: t('a), p2: t('a)) =>
    switch (p1.term, p2.term) {
    | (Parens(x), _) => fast_equal(x, p2)
    | (_, Parens(x)) => fast_equal(p1, x)
    | (EmptyHole, EmptyHole) => true
    | (MultiHole(xs), MultiHole(ys)) =>
      List.length(xs) == List.length(ys)
      && List.equal(Any.fast_equal, xs, ys)
    | (Invalid(s1), Invalid(s2)) => s1 == s2
    | (Wild, Wild) => true
    | (Bool(b1), Bool(b2)) => b1 == b2
    | (Int(i1), Int(i2)) => i1 == i2
    | (Float(f1), Float(f2)) => f1 == f2
    | (String(s1), String(s2)) => s1 == s2
    | (Constructor(c1, t1), Constructor(c2, t2)) =>
      c1 == c2 && Typ.fast_equal(t1, t2)
    | (Var(v1), Var(v2)) => v1 == v2
    | (ListLit(xs), ListLit(ys)) =>
      List.length(xs) == List.length(ys) && List.equal(fast_equal, xs, ys)
    | (Cons(x1, y1), Cons(x2, y2)) =>
      fast_equal(x1, x2) && fast_equal(y1, y2)
    | (Tuple(xs), Tuple(ys)) =>
      List.length(xs) == List.length(ys) && List.equal(fast_equal, xs, ys)
    | (Ap(x1, y1), Ap(x2, y2)) => fast_equal(x1, x2) && fast_equal(y1, y2)
    | (Cast(x1, t1, t2), Cast(x2, u1, u2)) =>
      fast_equal(x1, x2) && Typ.fast_equal(t1, u1) && Typ.fast_equal(t2, u2)
    | (EmptyHole, _)
    | (MultiHole(_), _)
    | (Invalid(_), _)
    | (Wild, _)
    | (Bool(_), _)
    | (Int(_), _)
    | (Float(_), _)
    | (String(_), _)
    | (ListLit(_), _)
    | (Constructor(_), _)
    | (Cons(_), _)
    | (Var(_), _)
    | (Tuple(_), _)
    | (Ap(_), _)
    | (Cast(_), _) => false
    };
}
and Typ: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_hole('a) =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t('a)));

  /* TYPE_PROVENANCE: From whence does an unknown type originate?
     Is it generated from an unannotated pattern variable (SynSwitch),
     a pattern variable annotated with a type hole (TypeHole), or
     generated by an internal judgement (Internal)? */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_provenance('a) =
    | SynSwitch
    | Hole(type_hole('a))
    | Internal;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term('a) =
    | Unknown(Typ.type_provenance('a))
    | Int
    | Float
    | Bool
    | String
    | Var(string)
    | List(t('a))
    | Arrow(t('a), t('a))
    | Sum(ConstructorMap.t(t('a)))
    | Prod(list(t('a)))
    | Parens(t('a))
    | Ap(t('a), t('a))
    | Rec(TPat.t('a), t('a))
    | Forall(TPat.t('a), t('a))
  and t('a) = Annotated.t(term('a), 'a);

  type sum_map('a) = ConstructorMap.t(t('a));

  let map_term:
    (
      ~f_exp: (Exp.t('a) => Exp.t('a), Exp.t('a)) => Exp.t('a)=?,
      ~f_pat: (Pat.t('a) => Pat.t('a), Pat.t('a)) => Pat.t('a)=?,
      ~f_typ: (Typ.t('a) => Typ.t('a), Typ.t('a)) => Typ.t('a)=?,
      ~f_tpat: (TPat.t('a) => TPat.t('a), TPat.t('a)) => TPat.t('a)=?,
      ~f_rul: (Rul.t('a) => Rul.t('a), Rul.t('a)) => Rul.t('a)=?,
      ~f_any: (Any.t('a) => Any.t('a), Any.t('a)) => Any.t('a)=?,
      t('a)
    ) =>
    t('a);

  let subst: (t('a), TPat.t('a), t('a)) => t('a);

  let fast_equal: (t('a), t('a)) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_hole('a) =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t('a)));

  /* TYPE_PROVENANCE: From whence does an unknown type originate?
     Is it generated from an unannotated pattern variable (SynSwitch),
     a pattern variable annotated with a type hole (TypeHole), or
     generated by an internal judgement (Internal)? */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_provenance('a) =
    | SynSwitch
    | Hole(type_hole('a))
    | Internal;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term('a) =
    | Unknown(Typ.type_provenance('a))
    | Int
    | Float
    | Bool
    | String
    | Var(string)
    | List(t('a))
    | Arrow(t('a), t('a))
    | Sum(ConstructorMap.t(t('a)))
    | Prod(list(t('a)))
    | Parens(t('a))
    | Ap(t('a), t('a))
    | Rec(TPat.t('a), t('a))
    | Forall(TPat.t('a), t('a))
  and t('a) = Annotated.t(term('a), 'a);

  type sum_map('a) = ConstructorMap.t(t('a));

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
    let rec_call = ({term, _} as exp: t('a)) => {
      ...exp,
      term:
        switch (term) {
        | Unknown(Hole(EmptyHole))
        | Unknown(Hole(Invalid(_)))
        | Unknown(SynSwitch)
        | Unknown(Internal)
        | Bool
        | Int
        | Float
        | String
        | Var(_) => term
        | List(t) => List(typ_map_term(t))
        | Unknown(Hole(MultiHole(things))) =>
          Unknown(Hole(MultiHole(List.map(any_map_term, things))))
        | Ap(e1, e2) => Ap(typ_map_term(e1), typ_map_term(e2))
        | Prod(xs) => Prod(List.map(typ_map_term, xs))
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
        | Forall(tp, t) => Forall(tpat_map_term(tp), typ_map_term(t))
        },
    };
    x |> f_typ(rec_call);
  };

  let rec subst = (s, x, ty: t('a)) => {
    switch (TPat.tyvar_of_utpat(x)) {
    | Some(str) =>
      let (term, rewrap) = Annotated.unwrap(ty);
      switch (term) {
      | Int => Int |> rewrap
      | Float => Float |> rewrap
      | Bool => Bool |> rewrap
      | String => String |> rewrap
      | Unknown(prov) => Unknown(prov) |> rewrap
      | Arrow(ty1, ty2) =>
        Arrow(subst(s, x, ty1), subst(s, x, ty2)) |> rewrap
      | Prod(tys) => Prod(List.map(subst(s, x), tys)) |> rewrap
      | Sum(sm) =>
        Sum(ConstructorMap.map(Option.map(subst(s, x)), sm)) |> rewrap
      | Forall(tp2, ty)
          when TPat.tyvar_of_utpat(x) == TPat.tyvar_of_utpat(tp2) =>
        Forall(tp2, ty) |> rewrap
      | Forall(tp2, ty) => Forall(tp2, subst(s, x, ty)) |> rewrap
      | Rec(tp2, ty) when TPat.tyvar_of_utpat(x) == TPat.tyvar_of_utpat(tp2) =>
        Rec(tp2, ty) |> rewrap
      | Rec(tp2, ty) => Rec(tp2, subst(s, x, ty)) |> rewrap
      | List(ty) => List(subst(s, x, ty)) |> rewrap
      | Var(y) => str == y ? s : Var(y) |> rewrap
      | Parens(ty) => Parens(subst(s, x, ty)) |> rewrap
      | Ap(t1, t2) => Ap(subst(s, x, t1), subst(s, x, t2)) |> rewrap
      };
    | None => ty
    };
  };

  /* Type Equality: This coincides with alpha equivalence for normalized types.
     Other types may be equivalent but this will not detect so if they are not normalized. */

  let rec eq_internal = (n: int, t1: t('a), t2: t('a)): bool => {
    switch (t1.term, t2.term) {
    | (Parens(t1), _) => eq_internal(n, t1, t2)
    | (_, Parens(t2)) => eq_internal(n, t1, t2)
    | (Rec(x1, t1), Rec(x2, t2))
    | (Forall(x1, t1), Forall(x2, t2)) =>
      let foo: t('a) = {
        term: Var("=" ++ string_of_int(n)),
        annotation: t1.annotation // TODO I don't think this matters in this case. I'd like to just pass unit
      };
      let alpha_subst = subst(foo);
      eq_internal(n + 1, alpha_subst(x1, t1), alpha_subst(x2, t2));
    | (Rec(_), _) => false
    | (Forall(_), _) => false
    | (Int, Int) => true
    | (Int, _) => false
    | (Float, Float) => true
    | (Float, _) => false
    | (Bool, Bool) => true
    | (Bool, _) => false
    | (String, String) => true
    | (String, _) => false
    | (Ap(t1, t2), Ap(t1', t2')) =>
      eq_internal(n, t1, t1') && eq_internal(n, t2, t2')
    | (Ap(_), _) => false
    | (Unknown(_), Unknown(_)) => true
    | (Unknown(_), _) => false
    | (Arrow(t1, t2), Arrow(t1', t2')) =>
      eq_internal(n, t1, t1') && eq_internal(n, t2, t2')
    | (Arrow(_), _) => false
    | (Prod(tys1), Prod(tys2)) => List.equal(eq_internal(n), tys1, tys2)
    | (Prod(_), _) => false
    | (List(t1), List(t2)) => eq_internal(n, t1, t2)
    | (List(_), _) => false
    | (Sum(sm1), Sum(sm2)) =>
      /* Does not normalize the types. */
      ConstructorMap.equal(eq_internal(n), sm1, sm2)
    | (Sum(_), _) => false
    | (Var(n1), Var(n2)) => n1 == n2
    | (Var(_), _) => false
    };
  };

  let fast_equal = (x, y) => eq_internal(0, x, y);
}
and TPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term('a) =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t('a)))
    | Var(string)
  and t('a) = Annotated.t(term('a), 'a);

  let map_term:
    (
      ~f_exp: (Exp.t('a) => Exp.t('a), Exp.t('a)) => Exp.t('a)=?,
      ~f_pat: (Pat.t('a) => Pat.t('a), Pat.t('a)) => Pat.t('a)=?,
      ~f_typ: (Typ.t('a) => Typ.t('a), Typ.t('a)) => Typ.t('a)=?,
      ~f_tpat: (TPat.t('a) => TPat.t('a), TPat.t('a)) => TPat.t('a)=?,
      ~f_rul: (Rul.t('a) => Rul.t('a), Rul.t('a)) => Rul.t('a)=?,
      ~f_any: (Any.t('a) => Any.t('a), Any.t('a)) => Any.t('a)=?,
      t('a)
    ) =>
    t('a);

  let tyvar_of_utpat: t('a) => option(string);

  let fast_equal: (t('a), t('a)) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term('a) =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t('a)))
    | Var(string)
  and t('a) = Annotated.t(term('a), 'a);

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
    let rec_call = (exp: t('a)) => {
      ...exp,
      term:
        switch (exp.term) {
        | EmptyHole
        | Invalid(_)
        | Var(_) => exp.term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        },
    };
    x |> f_tpat(rec_call);
  };

  let tyvar_of_utpat = ({term, _}: t('a)) =>
    switch (term) {
    | Var(x) => Some(x)
    | _ => None
    };

  let fast_equal = (tp1: t('a), tp2: t('a)) =>
    switch (tp1.term, tp2.term) {
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
}
and Rul: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term('a) =
    | Invalid(string)
    | Hole(list(Any.t('a)))
    | Rules(Exp.t('a), list((Pat.t('a), Exp.t('a))))
  and t('a) = Annotated.t(term('a), 'a);

  let map_term:
    (
      ~f_exp: (Exp.t('a) => Exp.t('a), Exp.t('a)) => Exp.t('a)=?,
      ~f_pat: (Pat.t('a) => Pat.t('a), Pat.t('a)) => Pat.t('a)=?,
      ~f_typ: (Typ.t('a) => Typ.t('a), Typ.t('a)) => Typ.t('a)=?,
      ~f_tpat: (TPat.t('a) => TPat.t('a), TPat.t('a)) => TPat.t('a)=?,
      ~f_rul: (Rul.t('a) => Rul.t('a), Rul.t('a)) => Rul.t('a)=?,
      ~f_any: (Any.t('a) => Any.t('a), Any.t('a)) => Any.t('a)=?,
      t('a)
    ) =>
    t('a);

  let fast_equal: (t('a), t('a)) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term('a) =
    | Invalid(string)
    | Hole(list(Any.t('a)))
    | Rules(Exp.t('a), list((Pat.t('a), Exp.t('a))))
  and t('a) = Annotated.t(term('a), 'a);

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
    let rec_call = ({term, _} as exp: t('a)) => {
      ...exp,
      term:
        switch (term) {
        | Invalid(_) => term
        | Hole(things) => Hole(List.map(any_map_term, things))
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

  let fast_equal = (r1: t('a), r2: t('a)) =>
    switch (r1.term, r2.term) {
    | (Invalid(s1), Invalid(s2)) => s1 == s2
    | (Hole(xs), Hole(ys)) =>
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
    | (Hole(_), _)
    | (Rules(_), _) => false
    };
}

and Environment: {
  include
     (module type of VarBstMap.Ordered) with
      type t_('a) = VarBstMap.Ordered.t_('a);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = t_(Exp.t(IdTag.t));
} = {
  include VarBstMap.Ordered;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = t_(Exp.t(IdTag.t));
}

and ClosureEnvironment: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;

  let wrap: (Id.t, Environment.t) => t;

  let id_of: t => Id.t;
  let map_of: t => Environment.t;

  let to_list: t => list((Var.t, Exp.t(IdTag.t)));

  let of_environment: Environment.t => t;

  let id_equal: (t, t) => bool;

  let empty: t;
  let is_empty: t => bool;
  let length: t => int;

  let lookup: (t, Var.t) => option(Exp.t(IdTag.t));
  let contains: (t, Var.t) => bool;
  let update: (Environment.t => Environment.t, t) => t;
  let update_keep_id: (Environment.t => Environment.t, t) => t;
  let extend: (t, (Var.t, Exp.t(IdTag.t))) => t;
  let extend_keep_id: (t, (Var.t, Exp.t(IdTag.t))) => t;
  let union: (t, t) => t;
  let union_keep_id: (t, t) => t;
  let map: (((Var.t, Exp.t(IdTag.t))) => Exp.t(IdTag.t), t) => t;
  let map_keep_id: (((Var.t, Exp.t(IdTag.t))) => Exp.t(IdTag.t), t) => t;
  let filter: (((Var.t, Exp.t(IdTag.t))) => bool, t) => t;
  let filter_keep_id: (((Var.t, Exp.t(IdTag.t))) => bool, t) => t;
  let fold: (((Var.t, Exp.t(IdTag.t)), 'b) => 'b, 'b, t) => 'b;

  let without_keys: (list(Var.t), t) => t;

  let placeholder: t;
} = {
  module Inner: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    let wrap: (Id.t, Environment.t) => t;

    let id_of: t => Id.t;
    let map_of: t => Environment.t;
  } = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = (Id.t, Environment.t);

    let wrap = (ei, map): t => (ei, map);

    let id_of = ((ei, _)) => ei;
    let map_of = ((_, map)) => map;
    let (sexp_of_t, t_of_sexp) =
      StructureShareSexp.structure_share_here(id_of, sexp_of_t, t_of_sexp);
  };
  include Inner;

  let to_list = env => env |> map_of |> Environment.to_listo;

  let of_environment = map => {
    let ei = Id.mk();
    wrap(ei, map);
  };

  /* Equals only needs to check environment id's (faster than structural equality
   * checking.) */
  let id_equal = (env1, env2) => id_of(env1) == id_of(env2);

  let empty = Environment.empty |> of_environment;

  let is_empty = env => env |> map_of |> Environment.is_empty;

  let length = env => Environment.length(map_of(env));

  let lookup = (env, x) =>
    env |> map_of |> (map => Environment.lookup(map, x));

  let contains = (env, x) =>
    env |> map_of |> (map => Environment.contains(map, x));

  let update = (f, env) => env |> map_of |> f |> of_environment;

  let update_keep_id = (f, env) => env |> map_of |> f |> wrap(env |> id_of);

  let extend = (env, xr) =>
    env |> update(map => Environment.extend(map, xr));

  let extend_keep_id = (env, xr) =>
    env |> update_keep_id(map => Environment.extend(map, xr));

  let union = (env1, env2) =>
    env2 |> update(map2 => Environment.union(env1 |> map_of, map2));

  let union_keep_id = (env1, env2) =>
    env2 |> update_keep_id(map2 => Environment.union(env1 |> map_of, map2));

  let map = (f, env) => env |> update(Environment.mapo(f));

  let map_keep_id = (f, env) => env |> update_keep_id(Environment.mapo(f));

  let filter = (f, env) => env |> update(Environment.filtero(f));

  let filter_keep_id = (f, env) =>
    env |> update_keep_id(Environment.filtero(f));

  let fold = (f, init, env) => env |> map_of |> Environment.foldo(f, init);

  let placeholder = wrap(Id.invalid, Environment.empty);

  let without_keys = keys => update(Environment.without_keys(keys));
}
and StepperFilterKind: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type filter('a) = {
    pat: Exp.t('a),
    act: FilterAction.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('a) =
    | Filter(filter('a))
    | Residue(int, FilterAction.t);

  let map_term:
    (
      ~f_exp: (Exp.t('a) => Exp.t('a), Exp.t('a)) => Exp.t('a)=?,
      ~f_pat: (Pat.t('a) => Pat.t('a), Pat.t('a)) => Pat.t('a)=?,
      ~f_typ: (Typ.t('a) => Typ.t('a), Typ.t('a)) => Typ.t('a)=?,
      ~f_tpat: (TPat.t('a) => TPat.t('a), TPat.t('a)) => TPat.t('a)=?,
      ~f_rul: (Rul.t('a) => Rul.t('a), Rul.t('a)) => Rul.t('a)=?,
      ~f_any: (Any.t('a) => Any.t('a), Any.t('a)) => Any.t('a)=?,
      t('a)
    ) =>
    t('a);

  let map: (Exp.t('a) => Exp.t('a), t('a)) => t('a);

  let fast_equal: (t('a), t('a)) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type filter('a) = {
    pat: Exp.t('a),
    act: FilterAction.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('a) =
    | Filter(filter('a))
    | Residue(int, FilterAction.t);

  let map = (mapper, filter) => {
    switch (filter) {
    | Filter({act, pat}) => Filter({act, pat: mapper(pat)})
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
    fun
    | Filter({pat: e, act}) => Filter({pat: exp_map_term(e), act})
    | Residue(i, a) => Residue(i, a);
  };

  let fast_equal = (f1, f2) =>
    switch (f1, f2) {
    | (Filter({pat: e1, act: a1}), Filter({pat: e2, act: a2})) =>
      Exp.fast_equal(e1, e2) && a1 == a2
    | (Residue(i1, a1), Residue(i2, a2)) => i1 == i2 && a1 == a2
    | (Filter(_), _)
    | (Residue(_), _) => false
    };
};
