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

module rec Any: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Drv(Drv.t)
    | Exp(Exp.t)
    | Pat(Pat.t)
    | Typ(Typ.t)
    | TPat(TPat.t)
    | Rul(Rul.t)
    | Nul(unit)
    | Any(unit);

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_drv: Drv.mapper=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Drv(Drv.t)
    | Exp(Exp.t)
    | Pat(Pat.t)
    | Typ(Typ.t)
    | TPat(TPat.t)
    | Rul(Rul.t)
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
        ~f_drv=Drv.drv_continue,
        x,
      ) => {
    let pat_map_term =
      Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_drv);
    let rec_call = y =>
      switch (y) {
      | Drv(x) => Drv(Drv.map_term(~f_hazel_pat=pat_map_term, ~f_drv, x))
      | Exp(x) =>
        Exp(
          Exp.map_term(
            ~f_exp,
            ~f_pat,
            ~f_typ,
            ~f_tpat,
            ~f_rul,
            ~f_any,
            ~f_drv,
            x,
          ),
        )
      | Pat(x) => Pat(pat_map_term(x))
      | Typ(x) =>
        Typ(
          Typ.map_term(
            ~f_exp,
            ~f_pat,
            ~f_typ,
            ~f_tpat,
            ~f_rul,
            ~f_any,
            ~f_drv,
            x,
          ),
        )
      | TPat(x) =>
        TPat(
          TPat.map_term(
            ~f_exp,
            ~f_pat,
            ~f_typ,
            ~f_tpat,
            ~f_rul,
            ~f_any,
            ~f_drv,
            x,
          ),
        )
      | Rul(x) =>
        Rul(
          Rul.map_term(
            ~f_exp,
            ~f_pat,
            ~f_typ,
            ~f_tpat,
            ~f_rul,
            ~f_any,
            ~f_drv,
            x,
          ),
        )
      | Nul () => Nul()
      | Any () => Any()
      };
    x |> f_any(rec_call);
  };

  let fast_equal = (x, y) =>
    switch (x, y) {
    | (Drv(x), Drv(y)) => Drv.fast_equal(x, y)
    | (Exp(x), Exp(y)) => Exp.fast_equal(x, y)
    | (Pat(x), Pat(y)) => Pat.fast_equal(x, y)
    | (Typ(x), Typ(y)) => Typ.fast_equal(x, y)
    | (TPat(x), TPat(y)) => TPat.fast_equal(x, y)
    | (Rul(x), Rul(y)) => Rul.fast_equal(x, y)
    | (Nul (), Nul ()) => true
    | (Any (), Any ()) => true
    | (Drv(_), _)
    | (Exp(_), _)
    | (Pat(_), _)
    | (Typ(_), _)
    | (TPat(_), _)
    | (Rul(_), _)
    | (Nul (), _)
    | (Any (), _) => false
    };
}
and TypeHole: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t));
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t));
}
and Exp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type deferral_position =
    | InAp
    | OutsideAp;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | DynamicErrorHole(t, InvalidOperationError.t)
    | FailedCast(t, Typ.t, Typ.t)
    | Deferral(deferral_position)
    | Undefined
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | Term(Any.t, Sort.t)
    | ListLit(list(t))
    | Constructor(string, Typ.t) // Typ.t field is only meaningful in dynamic expressions
    | Fun(
        Pat.t,
        t,
        [@show.opaque] option(ClosureEnvironment.t),
        option(Var.t),
      )
    | TypFun(TPat.t, t, option(Var.t))
    | Tuple(list(t))
    | Var(Var.t)
    | Let(Pat.t, t, t)
    | FixF(Pat.t, t, option(ClosureEnvironment.t))
    | TyAlias(TPat.t, Typ.t, t)
    | Ap(Operators.ap_direction, t, t)
    | TypAp(t, Typ.t)
    | DeferredAp(t, list(t))
    | If(t, t, t)
    | Seq(t, t)
    | Test(t)
    | Filter(StepperFilterKind.t, t)
    | Closure([@show.opaque] ClosureEnvironment.t, t)
    | Parens(t) // (
    | Cons(t, t)
    | ListConcat(t, t)
    | UnOp(Operators.op_un, t)
    | BinOp(Operators.op_bin, t, t)
    | BuiltinFun(string)
    | Match(t, list((Pat.t, t)))
    /* INVARIANT: in dynamic expressions, casts must be between
       two consistent types. Both types should be normalized in
       dynamics for the cast calculus to work right. */
    | Cast(t, Typ.t, Typ.t) // first Typ.t field is only meaningful in dynamic expressions
  and t = IdTagged.t(term);

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_drv: Drv.mapper=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type deferral_position =
    | InAp
    | OutsideAp;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | DynamicErrorHole(t, InvalidOperationError.t)
    | FailedCast(t, Typ.t, Typ.t)
    | Deferral(deferral_position)
    | Undefined
    | Bool(bool)
    | Int(int)
    | Float(float)
    | String(string)
    | Term(Any.t, Sort.t)
    | ListLit(list(t))
    | Constructor(string, Typ.t)
    | Fun(
        Pat.t,
        t,
        [@show.opaque] option(ClosureEnvironment.t),
        option(Var.t),
      )
    | TypFun(TPat.t, t, option(string))
    | Tuple(list(t))
    | Var(Var.t)
    | Let(Pat.t, t, t)
    | FixF(Pat.t, t, [@show.opaque] option(ClosureEnvironment.t))
    | TyAlias(TPat.t, Typ.t, t)
    | Ap(Operators.ap_direction, t, t) // note: function is always first then argument; even in pipe mode
    | TypAp(t, Typ.t)
    | DeferredAp(t, list(t))
    | If(t, t, t)
    | Seq(t, t)
    | Test(t)
    | Filter(StepperFilterKind.t, t)
    | Closure([@show.opaque] ClosureEnvironment.t, t)
    | Parens(t)
    | Cons(t, t)
    | ListConcat(t, t)
    | UnOp(Operators.op_un, t)
    | BinOp(Operators.op_bin, t, t)
    | BuiltinFun(string) /// Doesn't currently have a distinguishable syntax
    | Match(t, list((Pat.t, t)))
    | Cast(t, Typ.t, Typ.t)
  and t = IdTagged.t(term);

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        ~f_drv=Drv.drv_continue,
        x,
      ) => {
    let exp_map_term =
      Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_drv);
    let pat_map_term =
      Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_drv);
    let typ_map_term =
      Typ.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_drv);
    let tpat_map_term =
      TPat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_drv);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_drv);
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
        | Bool(_)
        | Int(_)
        | Float(_)
        | Constructor(_)
        | String(_)
        | Deferral(_)
        | Var(_)
        | Undefined => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | DynamicErrorHole(e, err) => DynamicErrorHole(exp_map_term(e), err)
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
        | Term(e, s) => Term(any_map_term(e), s)
        },
    };
    x |> f_exp(rec_call);
  };

  let rec fast_equal = (e1, e2) =>
    switch (e1 |> IdTagged.term_of, e2 |> IdTagged.term_of) {
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
    | (Term(t1, s1), Term(t2, s2)) => Any.fast_equal(t1, t2) && s1 == s2
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
    | (Term(_), _)
    | (MultiHole(_), _)
    | (EmptyHole, _)
    | (Undefined, _) => false
    };
}
and Pat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | String(string)
    | ListLit(list(t))
    | Constructor(string, Typ.t) // Typ.t field is only meaningful in dynamic patterns
    | Cons(t, t)
    | Var(Var.t)
    | Tuple(list(t))
    | Parens(t)
    | Ap(t, t)
    | Cast(t, Typ.t, Typ.t) // The second Typ.t field is only meaningful in dynamic patterns
  and t = IdTagged.t(term);

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_drv: Drv.mapper=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Wild
    | Int(int)
    | Float(float)
    | Bool(bool)
    | String(string)
    | ListLit(list(t))
    | Constructor(string, Typ.t)
    | Cons(t, t)
    | Var(Var.t)
    | Tuple(list(t))
    | Parens(t)
    | Ap(t, t)
    | Cast(t, Typ.t, Typ.t) // The second one is hidden from the user
  and t = IdTagged.t(term);

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        ~f_drv=Drv.drv_continue,
        x,
      ) => {
    let pat_map_term =
      Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_drv);
    let typ_map_term =
      Typ.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_drv);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_drv);
    let rec_call = ({term, _} as exp: t) => {
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

  let rec fast_equal = (p1, p2) =>
    switch (p1 |> IdTagged.term_of, p2 |> IdTagged.term_of) {
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
  /* TYPE_PROVENANCE: From whence does an unknown type originate?
     Is it generated from an unannotated pattern variable (SynSwitch),
     a pattern variable annotated with a type hole (TypeHole), or
     generated by an internal judgement (Internal)? */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_provenance =
    | SynSwitch
    | Hole(TypeHole.t)
    | Internal;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Unknown(Typ.type_provenance)
    | Int
    | Float
    | Bool
    | String
    | Term(Sort.t)
    | Var(string)
    | List(t)
    | Arrow(t, t)
    | Sum(ConstructorMap.t(t))
    | Prod(list(t))
    | Parens(t)
    | Ap(t, t)
    | Rec(TPat.t, t)
    | Forall(TPat.t, t)
  and t = IdTagged.t(term);

  type sum_map = ConstructorMap.t(t);

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_drv: Drv.mapper=?,
      t
    ) =>
    t;

  let subst: (t, TPat.t, t) => t;

  let fast_equal: (t, t) => bool;
} = {
  /* TYPE_PROVENANCE: From whence does an unknown type originate?
     Is it generated from an unannotated pattern variable (SynSwitch),
     a pattern variable annotated with a type hole (TypeHole), or
     generated by an internal judgement (Internal)? */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_provenance =
    | SynSwitch
    | Hole(TypeHole.t)
    | Internal;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Unknown(Typ.type_provenance)
    | Int
    | Float
    | Bool
    | String
    | Term(Sort.t)
    | Var(string)
    | List(t)
    | Arrow(t, t)
    | Sum(ConstructorMap.t(t))
    | Prod(list(t))
    | Parens(t)
    | Ap(t, t)
    | Rec(TPat.t, t)
    | Forall(TPat.t, t)
  and t = IdTagged.t(term);

  type sum_map = ConstructorMap.t(t);

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        ~f_drv=Drv.drv_continue,
        x,
      ) => {
    let typ_map_term =
      Typ.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_drv);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_drv);
    let tpat_map_term =
      TPat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_drv);
    let rec_call = ({term, _} as exp: t) => {
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
        | Term(_)
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

  let rec subst = (s: t, x: TPat.t, ty: t) => {
    switch (TPat.tyvar_of_utpat(x)) {
    | Some(str) =>
      let (term, rewrap) = IdTagged.unwrap(ty);
      switch (term) {
      | Int => Int |> rewrap
      | Float => Float |> rewrap
      | Bool => Bool |> rewrap
      | String => String |> rewrap
      | Term(sort) => Term(sort) |> rewrap
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

  let rec eq_internal = (n: int, t1: t, t2: t) => {
    switch (IdTagged.term_of(t1), IdTagged.term_of(t2)) {
    | (Parens(t1), _) => eq_internal(n, t1, t2)
    | (_, Parens(t2)) => eq_internal(n, t1, t2)
    | (Rec(x1, t1), Rec(x2, t2))
    | (Forall(x1, t1), Forall(x2, t2)) =>
      let alpha_subst =
        subst({
          term: Var("=" ++ string_of_int(n)),
          copied: false,
          ids: [Id.invalid],
        });
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
    | (Term(s1), Term(s2)) => s1 == s2
    | (Term(_), _) => false
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

  let fast_equal = eq_internal(0);
}
and TPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Var(string)
  and t = IdTagged.t(term);

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_drv: Drv.mapper=?,
      t
    ) =>
    t;

  let tyvar_of_utpat: t => option(string);

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | EmptyHole
    | MultiHole(list(Any.t))
    | Var(string)
  and t = IdTagged.t(term);

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        ~f_drv=Drv.drv_continue,
        x,
      ) => {
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_drv);
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
    switch (tp1 |> IdTagged.term_of, tp2 |> IdTagged.term_of) {
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
  type term =
    | Invalid(string)
    | Hole(list(Any.t))
    | Rules(Exp.t, list((Pat.t, Exp.t)))
  and t = IdTagged.t(term);

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_drv: Drv.mapper=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid(string)
    | Hole(list(Any.t))
    | Rules(Exp.t, list((Pat.t, Exp.t)))
  and t = IdTagged.t(term);

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        ~f_drv=Drv.drv_continue,
        x,
      ) => {
    let exp_map_term =
      Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_drv);
    let pat_map_term =
      Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_drv);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_drv);
    let rec_call = ({term, _} as exp: t) => {
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

  let fast_equal = (r1: t, r2: t) =>
    switch (r1 |> IdTagged.term_of, r2 |> IdTagged.term_of) {
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
  type t = t_(Exp.t);
} = {
  include VarBstMap.Ordered;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = t_(Exp.t);
}

and ClosureEnvironment: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;

  let wrap: (Id.t, Environment.t) => t;

  let id_of: t => Id.t;
  let map_of: t => Environment.t;

  let to_list: t => list((Var.t, Exp.t));

  let of_environment: Environment.t => t;

  let id_equal: (t, t) => bool;

  let empty: t;
  let is_empty: t => bool;
  let length: t => int;

  let lookup: (t, Var.t) => option(Exp.t);
  let contains: (t, Var.t) => bool;
  let update: (Environment.t => Environment.t, t) => t;
  let update_keep_id: (Environment.t => Environment.t, t) => t;
  let extend: (t, (Var.t, Exp.t)) => t;
  let extend_keep_id: (t, (Var.t, Exp.t)) => t;
  let union: (t, t) => t;
  let union_keep_id: (t, t) => t;
  let map: (((Var.t, Exp.t)) => Exp.t, t) => t;
  let map_keep_id: (((Var.t, Exp.t)) => Exp.t, t) => t;
  let filter: (((Var.t, Exp.t)) => bool, t) => t;
  let filter_keep_id: (((Var.t, Exp.t)) => bool, t) => t;
  let fold: (((Var.t, Exp.t), 'b) => 'b, 'b, t) => 'b;

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
  type filter = {
    pat: Exp.t,
    act: FilterAction.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Filter(filter)
    | Residue(int, FilterAction.t);

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      ~f_drv: Drv.mapper=?,
      t
    ) =>
    t;

  let map: (Exp.t => Exp.t, t) => t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type filter = {
    pat: Exp.t,
    act: FilterAction.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Filter(filter)
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
        ~f_drv=Drv.drv_continue,
      ) => {
    let exp_map_term =
      Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, ~f_drv);
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
}
and Drv: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Exp(ALFA_Exp.t)
    | Rul(ALFA_Rul.t)
    | Pat(ALFA_Pat.t)
    | Typ(ALFA_Typ.t)
    | TPat(ALFA_TPat.t);

  type mapper = {
    f_exp: (ALFA_Exp.t => ALFA_Exp.t, ALFA_Exp.t) => ALFA_Exp.t,
    f_rul: (ALFA_Rul.t => ALFA_Rul.t, ALFA_Rul.t) => ALFA_Rul.t,
    f_pat: (ALFA_Pat.t => ALFA_Pat.t, ALFA_Pat.t) => ALFA_Pat.t,
    f_typ: (ALFA_Typ.t => ALFA_Typ.t, ALFA_Typ.t) => ALFA_Typ.t,
    f_tpat: (ALFA_TPat.t => ALFA_TPat.t, ALFA_TPat.t) => ALFA_TPat.t,
  };

  let drv_continue: mapper;

  let map_term: (~f_hazel_pat: Pat.t => Pat.t=?, ~f_drv: mapper=?, t) => t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Exp(ALFA_Exp.t)
    | Rul(ALFA_Rul.t)
    | Pat(ALFA_Pat.t)
    | Typ(ALFA_Typ.t)
    | TPat(ALFA_TPat.t);

  type mapper = {
    f_exp: (ALFA_Exp.t => ALFA_Exp.t, ALFA_Exp.t) => ALFA_Exp.t,
    f_rul: (ALFA_Rul.t => ALFA_Rul.t, ALFA_Rul.t) => ALFA_Rul.t,
    f_pat: (ALFA_Pat.t => ALFA_Pat.t, ALFA_Pat.t) => ALFA_Pat.t,
    f_typ: (ALFA_Typ.t => ALFA_Typ.t, ALFA_Typ.t) => ALFA_Typ.t,
    f_tpat: (ALFA_TPat.t => ALFA_TPat.t, ALFA_TPat.t) => ALFA_TPat.t,
  };

  let drv_continue = {
    f_exp: continue,
    f_rul: continue,
    f_pat: continue,
    f_typ: continue,
    f_tpat: continue,
  };

  let map_term = (~f_hazel_pat=continue, ~f_drv=drv_continue, x: t) => {
    let {f_exp, f_rul, f_pat, f_typ, f_tpat} = f_drv;
    switch (x) {
    | Exp(exp) =>
      Exp(
        ALFA_Exp.map_term(~f_hazel_pat, ~f_exp, ~f_pat, ~f_typ, ~f_tpat, exp),
      )
    | Rul(rul) =>
      Rul(
        ALFA_Rul.map_term(
          ~f_hazel_pat,
          ~f_exp,
          ~f_rul,
          ~f_pat,
          ~f_typ,
          ~f_tpat,
          rul,
        ),
      )
    | Pat(pat) => Pat(ALFA_Pat.map_term(~f_pat, ~f_typ, ~f_tpat, pat))
    | Typ(typ) => Typ(ALFA_Typ.map_term(~f_typ, ~f_tpat, typ))
    | TPat(tpat) => TPat(ALFA_TPat.map_term(~f_tpat, tpat))
    };
  };

  let fast_equal = (x, y) =>
    switch (x, y) {
    | (Exp(e1), Exp(e2)) => ALFA_Exp.fast_equal(e1, e2)
    | (Exp(_), _) => false
    | (Rul(r1), Rul(r2)) => ALFA_Rul.fast_equal(r1, r2)
    | (Rul(_), _) => false
    | (Pat(p1), Pat(p2)) => ALFA_Pat.fast_equal(p1, p2)
    | (Pat(_), _) => false
    | (Typ(t1), Typ(t2)) => ALFA_Typ.fast_equal(t1, t2)
    | (Typ(_), _) => false
    | (TPat(tp1), TPat(tp2)) => ALFA_TPat.fast_equal(tp1, tp2)
    | (TPat(_), _) => false
    };
}
and ALFA_Exp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Hole(TypeHole.t)
    | Var(Var.t) // Prop / Exp
    | Abbr(Pat.t) // Jdmt / Ctxt / Prop / Exp
    | Parens(t) // Jdmt / Ctxt / Prop / Exp
    | Tuple(list(t)) // [invalid] / Exp
    // Jdmt
    | Val(t)
    | Eval(t, t)
    | Entail(t, t)
    // Ctx
    | Ctx(list(t))
    | Cons(t, t)
    | Concat(t, t)
    // Prop
    | Type(ALFA_Typ.t)
    | HasType(t, ALFA_Typ.t)
    | Syn(t, ALFA_Typ.t)
    | Ana(t, ALFA_Typ.t)
    | And(t, t)
    | Or(t, t)
    | Impl(t, t)
    | Truth
    | Falsity
    // Exp
    | NumLit(int)
    | Neg(t)
    | Plus(t, t)
    | Minus(t, t)
    | Times(t, t)
    | Gt(t, t)
    | Lt(t, t)
    | Eq(t, t)
    | True
    | False
    | If(t, t, t)
    | Let(ALFA_Pat.t, t, t)
    | Fix(ALFA_Pat.t, t)
    | Fun(ALFA_Pat.t, t)
    | Ap(t, t)
    | Triv
    | PrjL(t)
    | PrjR(t)
    | InjL
    | InjR
    | Case(t, list((ALFA_Pat.t, t)))
    | Roll
    | Unroll
  and t = IdTagged.t(term);

  let map_term:
    (
      ~f_hazel_pat: Pat.t => Pat.t=?,
      ~f_exp: (ALFA_Exp.t => ALFA_Exp.t, ALFA_Exp.t) => ALFA_Exp.t=?,
      ~f_pat: (ALFA_Pat.t => ALFA_Pat.t, ALFA_Pat.t) => ALFA_Pat.t=?,
      ~f_typ: (ALFA_Typ.t => ALFA_Typ.t, ALFA_Typ.t) => ALFA_Typ.t=?,
      ~f_tpat: (ALFA_TPat.t => ALFA_TPat.t, ALFA_TPat.t) => ALFA_TPat.t=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Hole(TypeHole.t)
    | Var(Var.t) // Prop / Exp
    | Abbr(Pat.t) // Jdmt / Ctxt / Prop / Exp
    | Parens(t) // Jdmt / Ctxt / Prop / Exp
    | Tuple(list(t)) // [invalid] / Exp
    // Jdmt
    | Val(t)
    | Eval(t, t)
    | Entail(t, t)
    // Ctx
    | Ctx(list(t))
    | Cons(t, t)
    | Concat(t, t)
    // Prop
    | Type(ALFA_Typ.t)
    | HasType(t, ALFA_Typ.t)
    | Syn(t, ALFA_Typ.t)
    | Ana(t, ALFA_Typ.t)
    | And(t, t)
    | Or(t, t)
    | Impl(t, t)
    | Truth
    | Falsity
    // Exp
    | NumLit(int)
    | Neg(t)
    | Plus(t, t)
    | Minus(t, t)
    | Times(t, t)
    | Gt(t, t)
    | Lt(t, t)
    | Eq(t, t)
    | True
    | False
    | If(t, t, t)
    | Let(ALFA_Pat.t, t, t)
    | Fix(ALFA_Pat.t, t)
    | Fun(ALFA_Pat.t, t)
    | Ap(t, t)
    | Triv
    | PrjL(t)
    | PrjR(t)
    | InjL
    | InjR
    | Case(t, list((ALFA_Pat.t, t)))
    | Roll
    | Unroll
  and t = IdTagged.t(term);

  let map_term =
      (
        ~f_hazel_pat=continue,
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        x,
      ) => {
    let exp_map_term =
      ALFA_Exp.map_term(~f_hazel_pat, ~f_exp, ~f_pat, ~f_typ, ~f_tpat);
    let pat_map_term =
      ALFA_Pat.map_term(~f_hazel_pat, ~f_pat, ~f_typ, ~f_tpat);
    let typ_map_term = ALFA_Typ.map_term(~f_hazel_pat, ~f_typ, ~f_tpat);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Hole(_) => term
        | Var(v) => Var(v)
        | Abbr(e) => Abbr(f_hazel_pat(e))
        | Parens(e) => Parens(exp_map_term(e))
        | Val(e) => Val(exp_map_term(e))
        | Eval(e1, e2) => Eval(exp_map_term(e1), exp_map_term(e2))
        | Entail(e1, e2) => Entail(exp_map_term(e1), exp_map_term(e2))
        | Ctx(e) => Ctx(List.map(exp_map_term, e))
        | Cons(e1, e2) => Cons(exp_map_term(e1), exp_map_term(e2))
        | Concat(e1, e2) => Concat(exp_map_term(e1), exp_map_term(e2))
        | Type(t) => Type(typ_map_term(t))
        | HasType(e, t) => HasType(exp_map_term(e), typ_map_term(t))
        | Syn(e, t) => Syn(exp_map_term(e), typ_map_term(t))
        | Ana(e, t) => Ana(exp_map_term(e), typ_map_term(t))
        | And(e1, e2) => And(exp_map_term(e1), exp_map_term(e2))
        | Or(e1, e2) => Or(exp_map_term(e1), exp_map_term(e2))
        | Impl(e1, e2) => Impl(exp_map_term(e1), exp_map_term(e2))
        | Truth => Truth
        | Falsity => Falsity
        | Tuple(es) => Tuple(List.map(exp_map_term, es))
        | NumLit(n) => NumLit(n)
        | Neg(e) => Neg(exp_map_term(e))
        | Plus(e1, e2) => Plus(exp_map_term(e1), exp_map_term(e2))
        | Minus(e1, e2) => Minus(exp_map_term(e1), exp_map_term(e2))
        | Times(e1, e2) => Times(exp_map_term(e1), exp_map_term(e2))
        | Gt(e1, e2) => Gt(exp_map_term(e1), exp_map_term(e2))
        | Lt(e1, e2) => Lt(exp_map_term(e1), exp_map_term(e2))
        | Eq(e1, e2) => Eq(exp_map_term(e1), exp_map_term(e2))
        | True => True
        | False => False
        | If(e1, e2, e3) =>
          If(exp_map_term(e1), exp_map_term(e2), exp_map_term(e3))
        | Let(p, e1, e2) =>
          Let(pat_map_term(p), exp_map_term(e1), exp_map_term(e2))
        | Fix(p, e) => Fix(pat_map_term(p), exp_map_term(e))
        | Fun(p, e) => Fun(pat_map_term(p), exp_map_term(e))
        | Ap(e1, e2) => Ap(exp_map_term(e1), exp_map_term(e2))
        | Triv => Triv
        | PrjL(e) => PrjL(exp_map_term(e))
        | PrjR(e) => PrjR(exp_map_term(e))
        | InjL => InjL
        | InjR => InjR
        | Case(e, rls) =>
          Case(
            exp_map_term(e),
            List.map(
              ((p, e)) => (pat_map_term(p), exp_map_term(e)),
              rls,
            ),
          )
        | Roll => Roll
        | Unroll => Unroll
        },
    };
    x |> f_exp(rec_call);
  };

  let fast_equal = (x, y) =>
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Hole(_), _) => false
    | (Var(v1), Var(v2)) => v1 == v2
    | (Var(_), _) => false
    | (Abbr(p1), Abbr(p2)) => Pat.fast_equal(p1, p2)
    | (Abbr(_), _) => false
    | (Parens(e1), Parens(e2)) => ALFA_Exp.fast_equal(e1, e2)
    | (Parens(_), _) => false
    | (Val(e1), Val(e2)) => ALFA_Exp.fast_equal(e1, e2)
    | (Val(_), _) => false
    | (Eval(e11, e12), Eval(e21, e22)) =>
      ALFA_Exp.fast_equal(e11, e21) && ALFA_Exp.fast_equal(e12, e22)
    | (Eval(_), _) => false
    | (Entail(e11, e12), Entail(e21, e22)) =>
      ALFA_Exp.fast_equal(e11, e21) && ALFA_Exp.fast_equal(e12, e22)
    | (Entail(_), _) => false
    | (Ctx(es1), Ctx(es2)) =>
      List.length(es1) == List.length(es2)
      && List.for_all2(ALFA_Exp.fast_equal, es1, es2)
    | (Ctx(_), _) => false
    | (Cons(e11, e12), Cons(e21, e22)) =>
      ALFA_Exp.fast_equal(e11, e21) && ALFA_Exp.fast_equal(e12, e22)
    | (Cons(_), _) => false
    | (Concat(e11, e12), Concat(e21, e22)) =>
      ALFA_Exp.fast_equal(e11, e21) && ALFA_Exp.fast_equal(e12, e22)
    | (Concat(_), _) => false
    | (Type(t1), Type(t2)) => ALFA_Typ.fast_equal(t1, t2)
    | (Type(_), _) => false
    | (HasType(e1, t1), HasType(e2, t2)) =>
      ALFA_Exp.fast_equal(e1, e2) && ALFA_Typ.fast_equal(t1, t2)
    | (HasType(_), _) => false
    | (Syn(e1, t1), Syn(e2, t2)) =>
      ALFA_Exp.fast_equal(e1, e2) && ALFA_Typ.fast_equal(t1, t2)
    | (Syn(_), _) => false
    | (Ana(e1, t1), Ana(e2, t2)) =>
      ALFA_Exp.fast_equal(e1, e2) && ALFA_Typ.fast_equal(t1, t2)
    | (Ana(_), _) => false
    | (And(e11, e12), And(e21, e22)) =>
      ALFA_Exp.fast_equal(e11, e21) && ALFA_Exp.fast_equal(e12, e22)
    | (And(_), _) => false
    | (Or(e11, e12), Or(e21, e22)) =>
      ALFA_Exp.fast_equal(e11, e21) && ALFA_Exp.fast_equal(e12, e22)
    | (Or(_), _) => false
    | (Impl(e11, e12), Impl(e21, e22)) =>
      ALFA_Exp.fast_equal(e11, e21) && ALFA_Exp.fast_equal(e12, e22)
    | (Impl(_), _) => false
    | (Truth, Truth) => true
    | (Truth, _) => false
    | (Falsity, Falsity) => true
    | (Falsity, _) => false
    | (Tuple(es1), Tuple(es2)) =>
      List.length(es1) == List.length(es2)
      && List.for_all2(ALFA_Exp.fast_equal, es1, es2)
    | (Tuple(_), _) => false
    | (NumLit(n1), NumLit(n2)) => n1 == n2
    | (NumLit(_), _) => false
    | (Neg(e1), Neg(e2)) => ALFA_Exp.fast_equal(e1, e2)
    | (Neg(_), _) => false
    | (Plus(e11, e12), Plus(e21, e22)) =>
      ALFA_Exp.fast_equal(e11, e21) && ALFA_Exp.fast_equal(e12, e22)
    | (Plus(_), _) => false
    | (Minus(e11, e12), Minus(e21, e22)) =>
      ALFA_Exp.fast_equal(e11, e21) && ALFA_Exp.fast_equal(e12, e22)
    | (Minus(_), _) => false
    | (Times(e11, e12), Times(e21, e22)) =>
      ALFA_Exp.fast_equal(e11, e21) && ALFA_Exp.fast_equal(e12, e22)
    | (Times(_), _) => false
    | (Gt(e11, e12), Gt(e21, e22)) =>
      ALFA_Exp.fast_equal(e11, e21) && ALFA_Exp.fast_equal(e12, e22)
    | (Gt(_), _) => false
    | (Lt(e11, e12), Lt(e21, e22)) =>
      ALFA_Exp.fast_equal(e11, e21) && ALFA_Exp.fast_equal(e12, e22)
    | (Lt(_), _) => false
    | (Eq(e11, e12), Eq(e21, e22)) =>
      ALFA_Exp.fast_equal(e11, e21) && ALFA_Exp.fast_equal(e12, e22)
    | (Eq(_), _) => false
    | (True, True) => true
    | (True, _) => false
    | (False, False) => true
    | (False, _) => false
    | (If(e1, e2, e3), If(e1', e2', e3')) =>
      ALFA_Exp.fast_equal(e1, e1')
      && ALFA_Exp.fast_equal(e2, e2')
      && ALFA_Exp.fast_equal(e3, e3')
    | (If(_), _) => false
    | (Let(p1, e11, e12), Let(p2, e21, e22)) =>
      ALFA_Pat.fast_equal(p1, p2)
      && ALFA_Exp.fast_equal(e11, e21)
      && ALFA_Exp.fast_equal(e12, e22)
    | (Let(_), _) => false
    | (Fix(p1, e1), Fix(p2, e2)) =>
      ALFA_Pat.fast_equal(p1, p2) && ALFA_Exp.fast_equal(e1, e2)
    | (Fix(_), _) => false
    | (Fun(p1, e1), Fun(p2, e2)) =>
      ALFA_Pat.fast_equal(p1, p2) && ALFA_Exp.fast_equal(e1, e2)
    | (Fun(_), _) => false
    | (Ap(e11, e12), Ap(e21, e22)) =>
      ALFA_Exp.fast_equal(e11, e21) && ALFA_Exp.fast_equal(e12, e22)
    | (Ap(_), _) => false
    | (Triv, Triv) => true
    | (Triv, _) => false
    | (PrjL(e1), PrjL(e2)) => ALFA_Exp.fast_equal(e1, e2)
    | (PrjL(_), _) => false
    | (PrjR(e1), PrjR(e2)) => ALFA_Exp.fast_equal(e1, e2)
    | (PrjR(_), _) => false
    | (InjL, InjL) => true
    | (InjL, _) => false
    | (InjR, InjR) => true
    | (InjR, _) => false
    | (Case(e1, rls1), Case(e2, rls2)) =>
      ALFA_Exp.fast_equal(e1, e2)
      && List.length(rls1) == List.length(rls2)
      && List.for_all2(
           ((p1, e1), (p2, e2)) =>
             ALFA_Pat.fast_equal(p1, p2) && ALFA_Exp.fast_equal(e1, e2),
           rls1,
           rls2,
         )
    | (Case(_), _) => false
    | (Roll, Roll) => true
    | (Roll, _) => false
    | (Unroll, Unroll) => true
    | (Unroll, _) => false
    };
}
and ALFA_Rul: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Hole(TypeHole.t)
    | Rules(ALFA_Exp.t, list((ALFA_Pat.t, ALFA_Exp.t)))
  and t = IdTagged.t(term);

  let map_term:
    (
      ~f_hazel_pat: Pat.t => Pat.t=?,
      ~f_exp: (ALFA_Exp.t => ALFA_Exp.t, ALFA_Exp.t) => ALFA_Exp.t=?,
      ~f_rul: (ALFA_Rul.t => ALFA_Rul.t, ALFA_Rul.t) => ALFA_Rul.t=?,
      ~f_pat: (ALFA_Pat.t => ALFA_Pat.t, ALFA_Pat.t) => ALFA_Pat.t=?,
      ~f_typ: (ALFA_Typ.t => ALFA_Typ.t, ALFA_Typ.t) => ALFA_Typ.t=?,
      ~f_tpat: (ALFA_TPat.t => ALFA_TPat.t, ALFA_TPat.t) => ALFA_TPat.t=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Hole(TypeHole.t)
    | Rules(ALFA_Exp.t, list((ALFA_Pat.t, ALFA_Exp.t)))
  and t = IdTagged.t(term);

  let map_term =
      (
        ~f_hazel_pat=continue,
        ~f_exp=continue,
        ~f_rul=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        x,
      ) => {
    let exp_map_term =
      ALFA_Exp.map_term(~f_hazel_pat, ~f_exp, ~f_pat, ~f_typ, ~f_tpat);
    let pat_map_term =
      ALFA_Pat.map_term(~f_hazel_pat, ~f_pat, ~f_typ, ~f_tpat);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Hole(_) => term
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
    switch (r1 |> IdTagged.term_of, r2 |> IdTagged.term_of) {
    | (Hole(_), _) => false
    | (Rules(e1, rls1), Rules(e2, rls2)) =>
      ALFA_Exp.fast_equal(e1, e2)
      && List.length(rls1) == List.length(rls2)
      && List.for_all2(
           ((p1, e1), (p2, e2)) =>
             ALFA_Pat.fast_equal(p1, p2) && ALFA_Exp.fast_equal(e1, e2),
           rls1,
           rls2,
         )
    | (Rules(_), _) => false
    };
}
and ALFA_Pat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Hole(TypeHole.t)
    | Var(Var.t)
    | Cast(t, ALFA_Typ.t)
    | InjL
    | InjR
    | Ap(t, t)
    | Pair(t, t)
    | Parens(t)
  and t = IdTagged.t(term);

  let map_term:
    (
      ~f_hazel_pat: Pat.t => Pat.t=?,
      ~f_pat: (ALFA_Pat.t => ALFA_Pat.t, ALFA_Pat.t) => ALFA_Pat.t=?,
      ~f_typ: (ALFA_Typ.t => ALFA_Typ.t, ALFA_Typ.t) => ALFA_Typ.t=?,
      ~f_tpat: (ALFA_TPat.t => ALFA_TPat.t, ALFA_TPat.t) => ALFA_TPat.t=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Hole(TypeHole.t)
    | Var(Var.t)
    | Cast(t, ALFA_Typ.t)
    | InjL
    | InjR
    | Ap(t, t)
    | Pair(t, t)
    | Parens(t)
  and t = IdTagged.t(term);

  let map_term =
      (
        ~f_hazel_pat=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        x,
      ) => {
    let pat_map_term = ALFA_Pat.map_term(~f_pat, ~f_typ, ~f_tpat);
    let typ_map_term = ALFA_Typ.map_term(~f_hazel_pat, ~f_typ, ~f_tpat);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Hole(_) => term
        | Var(v) => Var(v)
        | Cast(p, t) => Cast(pat_map_term(p), typ_map_term(t))
        | InjL => InjL
        | InjR => InjR
        | Ap(p1, p2) => Ap(pat_map_term(p1), pat_map_term(p2))
        | Pair(p1, p2) => Pair(pat_map_term(p1), pat_map_term(p2))
        | Parens(p) => Parens(pat_map_term(p))
        },
    };
    x |> f_pat(rec_call);
  };

  let fast_equal = (x, y) =>
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Hole(_), _) => false
    | (Var(v1), Var(v2)) => v1 == v2
    | (Var(_), _) => false
    | (Cast(p1, t1), Cast(p2, t2)) =>
      ALFA_Pat.fast_equal(p1, p2) && ALFA_Typ.fast_equal(t1, t2)
    | (Cast(_), _) => false
    | (InjL, InjL) => true
    | (InjL, _) => false
    | (InjR, InjR) => true
    | (InjR, _) => false
    | (Ap(p1, p2), Ap(p1', p2')) =>
      ALFA_Pat.fast_equal(p1, p1') && ALFA_Pat.fast_equal(p2, p2')
    | (Ap(_), _) => false
    | (Pair(p1, p2), Pair(p1', p2')) =>
      ALFA_Pat.fast_equal(p1, p1') && ALFA_Pat.fast_equal(p2, p2')
    | (Pair(_), _) => false
    | (Parens(p1), Parens(p2)) => ALFA_Pat.fast_equal(p1, p2)
    | (Parens(_), _) => false
    };
}
and ALFA_Typ: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Hole(TypeHole.t)
    | Abbr(Pat.t)
    | Num
    | Bool
    | Arrow(t, t)
    | Prod(t, t)
    | Unit
    | Sum(t, t)
    | Var(Var.t)
    | Rec(ALFA_TPat.t, t)
    | Parens(t)
  and t = IdTagged.t(term);

  let map_term:
    (
      ~f_hazel_pat: Pat.t => Pat.t=?,
      ~f_typ: (ALFA_Typ.t => ALFA_Typ.t, ALFA_Typ.t) => ALFA_Typ.t=?,
      ~f_tpat: (ALFA_TPat.t => ALFA_TPat.t, ALFA_TPat.t) => ALFA_TPat.t=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Hole(TypeHole.t)
    | Abbr(Pat.t)
    | Num
    | Bool
    | Arrow(t, t)
    | Prod(t, t)
    | Unit
    | Sum(t, t)
    | Var(Var.t)
    | Rec(ALFA_TPat.t, t)
    | Parens(t)
  and t = IdTagged.t(term);

  let map_term = (~f_hazel_pat=continue, ~f_typ=continue, ~f_tpat=continue, x) => {
    let typ_map_term = ALFA_Typ.map_term(~f_typ, ~f_tpat);
    let tpat_map_term = ALFA_TPat.map_term(~f_tpat);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Hole(_) => term
        | Abbr(e) => Abbr(f_hazel_pat(e))
        | Num => Num
        | Bool => Bool
        | Arrow(t1, t2) => Arrow(typ_map_term(t1), typ_map_term(t2))
        | Prod(t1, t2) => Prod(typ_map_term(t1), typ_map_term(t2))
        | Unit => Unit
        | Sum(t1, t2) => Sum(typ_map_term(t1), typ_map_term(t2))
        | Var(v) => Var(v)
        | Rec(tp, t) => Rec(tpat_map_term(tp), typ_map_term(t))
        | Parens(t) => Parens(typ_map_term(t))
        },
    };
    x |> f_typ(rec_call);
  };

  let fast_equal = (x, y) =>
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Hole(_), _) => false
    | (Abbr(p1), Abbr(p2)) => Pat.fast_equal(p1, p2)
    | (Abbr(_), _) => false
    | (Num, Num) => true
    | (Num, _) => false
    | (Bool, Bool) => true
    | (Bool, _) => false
    | (Arrow(t1, t2), Arrow(t1', t2')) =>
      ALFA_Typ.fast_equal(t1, t1') && ALFA_Typ.fast_equal(t2, t2')
    | (Arrow(_), _) => false
    | (Prod(t1, t2), Prod(t1', t2')) =>
      ALFA_Typ.fast_equal(t1, t1') && ALFA_Typ.fast_equal(t2, t2')
    | (Prod(_), _) => false
    | (Unit, Unit) => true
    | (Unit, _) => false
    | (Sum(t1, t2), Sum(t1', t2')) =>
      ALFA_Typ.fast_equal(t1, t1') && ALFA_Typ.fast_equal(t2, t2')
    | (Sum(_), _) => false
    | (Var(v1), Var(v2)) => v1 == v2
    | (Var(_), _) => false
    | (Rec(tp1, t1), Rec(tp2, t2)) =>
      ALFA_TPat.fast_equal(tp1, tp2) && ALFA_Typ.fast_equal(t1, t2)
    | (Rec(_), _) => false
    | (Parens(t1), Parens(t2)) => ALFA_Typ.fast_equal(t1, t2)
    | (Parens(_), _) => false
    };
}
and ALFA_TPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Hole(TypeHole.t)
    | Var(Var.t)
  and t = IdTagged.t(term);

  let map_term:
    (~f_tpat: (ALFA_TPat.t => ALFA_TPat.t, ALFA_TPat.t) => ALFA_TPat.t=?, t) =>
    t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Hole(TypeHole.t)
    | Var(Var.t)
  and t = IdTagged.t(term);

  let map_term = (~f_tpat=continue, x) => {
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Hole(_) => term
        | Var(v) => Var(v)
        },
    };
    x |> f_tpat(rec_call);
  };

  let fast_equal = (x, y) =>
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Hole(_), _) => false
    | (Var(v1), Var(v2)) => v1 == v2
    | (Var(_), _) => false
    };
};
