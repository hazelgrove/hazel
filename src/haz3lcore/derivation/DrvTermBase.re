open Util;

let continue = x => x;
let stop = (_, x) => x;
[@deriving (show({with_path: false}), sexp, yojson)]
type any_t =
  | Exp(exp_t)
  | Pat(pat_t)
  | Typ(typ_t)
  | TPat(tpat_t)
  | Rul(rul_t)
  | Any(unit)
and exp_term =
  | Hole(type_hole)
  | Var(Var.t) // Prop / Exp
  | Abbr(exp_t) // Jdmt / Ctxt / Prop / Exp
  | Parens(exp_t) // Jdmt / Ctxt / Prop / Exp
  | Tuple(list(exp_t)) // [invalid] / Exp
  // Jdmt
  | Val(exp_t)
  | Eval(exp_t, exp_t)
  | Entail(exp_t, exp_t)
  | Consistent(typ_t, typ_t)
  | MatchedArrow(typ_t, typ_t)
  | MatchedProd(typ_t, typ_t)
  | MatchedSum(typ_t, typ_t)
  // Ctx
  | Ctx(list(exp_t))
  | Cons(exp_t, exp_t)
  | Concat(exp_t, exp_t)
  // Prop
  | Type(typ_t)
  | HasType(exp_t, typ_t)
  | Syn(exp_t, typ_t)
  | Ana(exp_t, typ_t)
  | And(exp_t, exp_t)
  | Or(exp_t, exp_t)
  | Impl(exp_t, exp_t)
  | Truth
  | Falsity
  // Exp
  | NumLit(int)
  | Neg(exp_t)
  | Plus(exp_t, exp_t)
  | Minus(exp_t, exp_t)
  | Times(exp_t, exp_t)
  | Gt(exp_t, exp_t)
  | Lt(exp_t, exp_t)
  | Eq(exp_t, exp_t)
  | True
  | False
  | If(exp_t, exp_t, exp_t)
  | Let(pat_t, exp_t, exp_t)
  | Fix(pat_t, exp_t)
  | Fun(pat_t, exp_t)
  | Ap(exp_t, exp_t)
  | Triv
  | PrjL(exp_t)
  | PrjR(exp_t)
  | InjL(exp_t)
  | InjR(exp_t)
  | Case(exp_t, list((pat_t, exp_t)))
  | Roll(exp_t)
  | Unroll(exp_t)
  | ExpHole
and exp_t = IdTagged.t(exp_term)
and pat_term =
  | Hole(type_hole)
  | Var(Var.t)
  | Cast(pat_t, typ_t)
  | InjL(pat_t)
  | InjR(pat_t)
  | Pair(pat_t, pat_t)
  | Parens(pat_t)
and pat_t = IdTagged.t(pat_term)
and rul_term =
  | Hole(type_hole)
  | Rules(exp_t, list((pat_t, exp_t)))
and rul_t = IdTagged.t(rul_term)
and typ_term =
  | Hole(type_hole)
  | Abbr(typ_t)
  | Num
  | Bool
  | Arrow(typ_t, typ_t)
  | Prod(typ_t, typ_t)
  | Unit
  | Sum(typ_t, typ_t)
  | Var(string)
  | Rec(tpat_t, typ_t)
  | Parens(typ_t)
  | TypHole
and typ_t = IdTagged.t(typ_term)
and tpat_term =
  | Hole(type_hole)
  | Var(Var.t)
and tpat_t = IdTagged.t(tpat_term)
and type_hole =
  | AbbrNotVar
  | AbbrNotFound
  | AbbrNotDrvTerm
  | Invalid(string)
  | EmptyHole
  | MultiHole(list(any_t));

module rec Any: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = any_t;

  type mapper = {
    f_exp: (exp_t => exp_t, exp_t) => exp_t,
    f_rul: (rul_t => rul_t, rul_t) => rul_t,
    f_pat: (pat_t => pat_t, pat_t) => pat_t,
    f_typ: (typ_t => typ_t, typ_t) => typ_t,
    f_tpat: (tpat_t => tpat_t, tpat_t) => tpat_t,
  };

  let drv_continue: mapper;

  let map_term: (~f_drv: mapper=?, t) => t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = any_t;

  type mapper = {
    f_exp: (exp_t => exp_t, exp_t) => exp_t,
    f_rul: (rul_t => rul_t, rul_t) => rul_t,
    f_pat: (pat_t => pat_t, pat_t) => pat_t,
    f_typ: (typ_t => typ_t, typ_t) => typ_t,
    f_tpat: (tpat_t => tpat_t, tpat_t) => tpat_t,
  };

  let drv_continue = {
    f_exp: continue,
    f_rul: continue,
    f_pat: continue,
    f_typ: continue,
    f_tpat: continue,
  };

  let map_term = (~f_drv=drv_continue, x: t) => {
    let {f_exp, f_rul, f_pat, f_typ, f_tpat} = f_drv;
    switch (x) {
    | Exp(exp) => Exp(Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, exp))
    | Rul(rul) =>
      Rul(Rul.map_term(~f_exp, ~f_rul, ~f_pat, ~f_typ, ~f_tpat, rul))
    | Pat(pat) => Pat(Pat.map_term(~f_pat, ~f_typ, ~f_tpat, pat))
    | Typ(typ) => Typ(Typ.map_term(~f_typ, ~f_tpat, typ))
    | TPat(tpat) => TPat(TPat.map_term(~f_tpat, tpat))
    | Any(_) => Any()
    };
  };

  let fast_equal = (x, y) =>
    switch (x, y) {
    | (Exp(e1), Exp(e2)) => Exp.fast_equal(e1, e2)
    | (Exp(_), _) => false
    | (Rul(r1), Rul(r2)) => Rul.fast_equal(r1, r2)
    | (Rul(_), _) => false
    | (Pat(p1), Pat(p2)) => Pat.fast_equal(p1, p2)
    | (Pat(_), _) => false
    | (Typ(t1), Typ(t2)) => Typ.fast_equal(t1, t2)
    | (Typ(_), _) => false
    | (TPat(tp1), TPat(tp2)) => TPat.fast_equal(tp1, tp2)
    | (TPat(_), _) => false
    | (Any(_), Any(_)) => true
    | (Any(_), _) => false
    };
}
and Exp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = exp_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = exp_t;

  let map_term:
    (
      ~f_exp: (exp_t => exp_t, exp_t) => exp_t=?,
      ~f_pat: (pat_t => pat_t, pat_t) => pat_t=?,
      ~f_typ: (typ_t => typ_t, typ_t) => typ_t=?,
      ~f_tpat: (tpat_t => tpat_t, tpat_t) => tpat_t=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = exp_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = exp_t;

  let map_term =
      (~f_exp=continue, ~f_pat=continue, ~f_typ=continue, ~f_tpat=continue, x) => {
    let exp_map_term = Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat);
    let pat_map_term = Pat.map_term(~f_pat, ~f_typ, ~f_tpat);
    let typ_map_term = Typ.map_term(~f_typ, ~f_tpat);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Hole(_) => term
        | Var(v) => Var(v)
        | Abbr(e) => Abbr(exp_map_term(e))
        | Parens(e) => Parens(exp_map_term(e))
        | Val(e) => Val(exp_map_term(e))
        | Eval(e1, e2) => Eval(exp_map_term(e1), exp_map_term(e2))
        | Entail(e1, e2) => Entail(exp_map_term(e1), exp_map_term(e2))
        | Consistent(t1, t2) =>
          Consistent(typ_map_term(t1), typ_map_term(t2))
        | MatchedArrow(t1, t2) =>
          MatchedArrow(typ_map_term(t1), typ_map_term(t2))
        | MatchedProd(t1, t2) =>
          MatchedProd(typ_map_term(t1), typ_map_term(t2))
        | MatchedSum(t1, t2) =>
          MatchedSum(typ_map_term(t1), typ_map_term(t2))
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
        | InjL(e) => InjL(exp_map_term(e))
        | InjR(e) => InjR(exp_map_term(e))
        | Case(e, rls) =>
          Case(
            exp_map_term(e),
            List.map(
              ((p, e)) => (pat_map_term(p), exp_map_term(e)),
              rls,
            ),
          )
        | Roll(e) => Roll(exp_map_term(e))
        | Unroll(e) => Unroll(exp_map_term(e))
        | ExpHole => ExpHole
        },
    };
    x |> f_exp(rec_call);
  };

  let fast_equal = (x, y) =>
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Hole(_), _) => false
    | (Var(v1), Var(v2)) => v1 == v2
    | (Var(_), _) => false
    | (Abbr(p1), Abbr(p2)) => Exp.fast_equal(p1, p2)
    | (Abbr(_), _) => false
    | (Parens(e1), Parens(e2)) => Exp.fast_equal(e1, e2)
    | (Parens(_), _) => false
    | (Val(e1), Val(e2)) => Exp.fast_equal(e1, e2)
    | (Val(_), _) => false
    | (Eval(e11, e12), Eval(e21, e22)) =>
      Exp.fast_equal(e11, e21) && Exp.fast_equal(e12, e22)
    | (Eval(_), _) => false
    | (Entail(e11, e12), Entail(e21, e22)) =>
      Exp.fast_equal(e11, e21) && Exp.fast_equal(e12, e22)
    | (Entail(_), _) => false
    | (Consistent(t11, t12), Consistent(t21, t22)) =>
      Typ.fast_equal(t11, t21) && Typ.fast_equal(t12, t22)
    | (Consistent(_), _) => false
    | (MatchedArrow(t11, t12), MatchedArrow(t21, t22)) =>
      Typ.fast_equal(t11, t21) && Typ.fast_equal(t12, t22)
    | (MatchedArrow(_), _) => false
    | (MatchedProd(t11, t12), MatchedProd(t21, t22)) =>
      Typ.fast_equal(t11, t21) && Typ.fast_equal(t12, t22)
    | (MatchedProd(_), _) => false
    | (MatchedSum(t11, t12), MatchedSum(t21, t22)) =>
      Typ.fast_equal(t11, t21) && Typ.fast_equal(t12, t22)
    | (MatchedSum(_), _) => false
    | (Ctx(es1), Ctx(es2)) =>
      List.length(es1) == List.length(es2)
      && List.for_all2(Exp.fast_equal, es1, es2)
    | (Ctx(_), _) => false
    | (Cons(e11, e12), Cons(e21, e22)) =>
      Exp.fast_equal(e11, e21) && Exp.fast_equal(e12, e22)
    | (Cons(_), _) => false
    | (Concat(e11, e12), Concat(e21, e22)) =>
      Exp.fast_equal(e11, e21) && Exp.fast_equal(e12, e22)
    | (Concat(_), _) => false
    | (Type(t1), Type(t2)) => Typ.fast_equal(t1, t2)
    | (Type(_), _) => false
    | (HasType(e1, t1), HasType(e2, t2)) =>
      Exp.fast_equal(e1, e2) && Typ.fast_equal(t1, t2)
    | (HasType(_), _) => false
    | (Syn(e1, t1), Syn(e2, t2)) =>
      Exp.fast_equal(e1, e2) && Typ.fast_equal(t1, t2)
    | (Syn(_), _) => false
    | (Ana(e1, t1), Ana(e2, t2)) =>
      Exp.fast_equal(e1, e2) && Typ.fast_equal(t1, t2)
    | (Ana(_), _) => false
    | (And(e11, e12), And(e21, e22)) =>
      Exp.fast_equal(e11, e21) && Exp.fast_equal(e12, e22)
    | (And(_), _) => false
    | (Or(e11, e12), Or(e21, e22)) =>
      Exp.fast_equal(e11, e21) && Exp.fast_equal(e12, e22)
    | (Or(_), _) => false
    | (Impl(e11, e12), Impl(e21, e22)) =>
      Exp.fast_equal(e11, e21) && Exp.fast_equal(e12, e22)
    | (Impl(_), _) => false
    | (Truth, Truth) => true
    | (Truth, _) => false
    | (Falsity, Falsity) => true
    | (Falsity, _) => false
    | (Tuple(es1), Tuple(es2)) =>
      List.length(es1) == List.length(es2)
      && List.for_all2(Exp.fast_equal, es1, es2)
    | (Tuple(_), _) => false
    | (NumLit(n1), NumLit(n2)) => n1 == n2
    | (NumLit(_), _) => false
    | (Neg(e1), Neg(e2)) => Exp.fast_equal(e1, e2)
    | (Neg(_), _) => false
    | (Plus(e11, e12), Plus(e21, e22)) =>
      Exp.fast_equal(e11, e21) && Exp.fast_equal(e12, e22)
    | (Plus(_), _) => false
    | (Minus(e11, e12), Minus(e21, e22)) =>
      Exp.fast_equal(e11, e21) && Exp.fast_equal(e12, e22)
    | (Minus(_), _) => false
    | (Times(e11, e12), Times(e21, e22)) =>
      Exp.fast_equal(e11, e21) && Exp.fast_equal(e12, e22)
    | (Times(_), _) => false
    | (Gt(e11, e12), Gt(e21, e22)) =>
      Exp.fast_equal(e11, e21) && Exp.fast_equal(e12, e22)
    | (Gt(_), _) => false
    | (Lt(e11, e12), Lt(e21, e22)) =>
      Exp.fast_equal(e11, e21) && Exp.fast_equal(e12, e22)
    | (Lt(_), _) => false
    | (Eq(e11, e12), Eq(e21, e22)) =>
      Exp.fast_equal(e11, e21) && Exp.fast_equal(e12, e22)
    | (Eq(_), _) => false
    | (True, True) => true
    | (True, _) => false
    | (False, False) => true
    | (False, _) => false
    | (If(e1, e2, e3), If(e1', e2', e3')) =>
      Exp.fast_equal(e1, e1')
      && Exp.fast_equal(e2, e2')
      && Exp.fast_equal(e3, e3')
    | (If(_), _) => false
    | (Let(p1, e11, e12), Let(p2, e21, e22)) =>
      Pat.fast_equal(p1, p2)
      && Exp.fast_equal(e11, e21)
      && Exp.fast_equal(e12, e22)
    | (Let(_), _) => false
    | (Fix(p1, e1), Fix(p2, e2)) =>
      Pat.fast_equal(p1, p2) && Exp.fast_equal(e1, e2)
    | (Fix(_), _) => false
    | (Fun(p1, e1), Fun(p2, e2)) =>
      Pat.fast_equal(p1, p2) && Exp.fast_equal(e1, e2)
    | (Fun(_), _) => false
    | (Ap(e11, e12), Ap(e21, e22)) =>
      Exp.fast_equal(e11, e21) && Exp.fast_equal(e12, e22)
    | (Ap(_), _) => false
    | (Triv, Triv) => true
    | (Triv, _) => false
    | (PrjL(e1), PrjL(e2)) => Exp.fast_equal(e1, e2)
    | (PrjL(_), _) => false
    | (PrjR(e1), PrjR(e2)) => Exp.fast_equal(e1, e2)
    | (PrjR(_), _) => false
    | (InjL(e1), InjL(e2)) => Exp.fast_equal(e1, e2)
    | (InjL(_), _) => false
    | (InjR(e1), InjR(e2)) => Exp.fast_equal(e1, e2)
    | (InjR(_), _) => false
    | (Case(e1, rls1), Case(e2, rls2)) =>
      Exp.fast_equal(e1, e2)
      && List.length(rls1) == List.length(rls2)
      && List.for_all2(
           ((p1, e1), (p2, e2)) =>
             Pat.fast_equal(p1, p2) && Exp.fast_equal(e1, e2),
           rls1,
           rls2,
         )
    | (Case(_), _) => false
    | (Roll(e1), Roll(e2)) => Exp.fast_equal(e1, e2)
    | (Roll(_), _) => false
    | (Unroll(e1), Unroll(e2)) => Exp.fast_equal(e1, e2)
    | (Unroll(_), _) => false
    | (ExpHole, ExpHole) => true
    | (ExpHole, _) => false
    };
}
and Rul: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = rul_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = rul_t;

  let map_term:
    (
      ~f_exp: (exp_t => exp_t, exp_t) => exp_t=?,
      ~f_rul: (rul_t => rul_t, rul_t) => rul_t=?,
      ~f_pat: (pat_t => pat_t, pat_t) => pat_t=?,
      ~f_typ: (typ_t => typ_t, typ_t) => typ_t=?,
      ~f_tpat: (tpat_t => tpat_t, tpat_t) => tpat_t=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = rul_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = rul_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_rul=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        x,
      ) => {
    let exp_map_term = Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat);
    let pat_map_term = Pat.map_term(~f_pat, ~f_typ, ~f_tpat);
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
      Exp.fast_equal(e1, e2)
      && List.length(rls1) == List.length(rls2)
      && List.for_all2(
           ((p1, e1), (p2, e2)) =>
             Pat.fast_equal(p1, p2) && Exp.fast_equal(e1, e2),
           rls1,
           rls2,
         )
    | (Rules(_), _) => false
    };
}
and Pat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = pat_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = pat_t;
  let map_term:
    (
      ~f_pat: (pat_t => pat_t, pat_t) => pat_t=?,
      ~f_typ: (typ_t => typ_t, typ_t) => typ_t=?,
      ~f_tpat: (tpat_t => tpat_t, tpat_t) => tpat_t=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = pat_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = pat_t;

  let map_term = (~f_pat=continue, ~f_typ=continue, ~f_tpat=continue, x) => {
    let pat_map_term = Pat.map_term(~f_pat, ~f_typ, ~f_tpat);
    let typ_map_term = Typ.map_term(~f_typ, ~f_tpat);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Hole(_) => term
        | Var(v) => Var(v)
        | Cast(p, t) => Cast(pat_map_term(p), typ_map_term(t))
        | InjL(p) => InjL(pat_map_term(p))
        | InjR(p) => InjR(pat_map_term(p))
        | Pair(p1, p2) => Pair(pat_map_term(p1), pat_map_term(p2))
        | Parens(p) => Parens(pat_map_term(p))
        },
    };
    x |> f_pat(rec_call);
  };

  let fast_equal = (x: t, y: t) =>
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Hole(_), _) => false
    | (Var(v1), Var(v2)) => v1 == v2
    | (Var(_), _) => false
    | (Cast(p1, t1), Cast(p2, t2)) =>
      Pat.fast_equal(p1, p2) && Typ.fast_equal(t1, t2)
    | (Cast(_), _) => false
    | (InjL(p1), InjL(p2)) => Pat.fast_equal(p1, p2)
    | (InjL(_), _) => false
    | (InjR(p1), InjR(p2)) => Pat.fast_equal(p1, p2)
    | (InjR(_), _) => false
    | (Pair(p1, p2), Pair(p1', p2')) =>
      Pat.fast_equal(p1, p1') && Pat.fast_equal(p2, p2')
    | (Pair(_), _) => false
    | (Parens(p1), Parens(p2)) => Pat.fast_equal(p1, p2)
    | (Parens(_), _) => false
    };
}
and Typ: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = typ_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = typ_t;

  let map_term:
    (
      ~f_typ: (typ_t => typ_t, typ_t) => typ_t=?,
      ~f_tpat: (tpat_t => tpat_t, tpat_t) => tpat_t=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = typ_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = typ_t;

  let map_term = (~f_typ=continue, ~f_tpat=continue, x) => {
    let typ_map_term = Typ.map_term(~f_typ, ~f_tpat);
    let tpat_map_term = TPat.map_term(~f_tpat);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Hole(_) => term
        | Abbr(e) => Abbr(typ_map_term(e))
        | Num => Num
        | Bool => Bool
        | Arrow(t1, t2) => Arrow(typ_map_term(t1), typ_map_term(t2))
        | Prod(t1, t2) => Prod(typ_map_term(t1), typ_map_term(t2))
        | Unit => Unit
        | Sum(t1, t2) => Sum(typ_map_term(t1), typ_map_term(t2))
        | Var(v) => Var(v)
        | Rec(tp, t) => Rec(tpat_map_term(tp), typ_map_term(t))
        | Parens(t) => Parens(typ_map_term(t))
        | TypHole => TypHole
        },
    };
    x |> f_typ(rec_call);
  };

  let fast_equal = (x: t, y: t) =>
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Hole(_), _) => false
    | (Abbr(p1), Abbr(p2)) => Typ.fast_equal(p1, p2)
    | (Abbr(_), _) => false
    | (Num, Num) => true
    | (Num, _) => false
    | (Bool, Bool) => true
    | (Bool, _) => false
    | (Arrow(t1, t2), Arrow(t1', t2')) =>
      Typ.fast_equal(t1, t1') && Typ.fast_equal(t2, t2')
    | (Arrow(_), _) => false
    | (Prod(t1, t2), Prod(t1', t2')) =>
      Typ.fast_equal(t1, t1') && Typ.fast_equal(t2, t2')
    | (Prod(_), _) => false
    | (Unit, Unit) => true
    | (Unit, _) => false
    | (Sum(t1, t2), Sum(t1', t2')) =>
      Typ.fast_equal(t1, t1') && Typ.fast_equal(t2, t2')
    | (Sum(_), _) => false
    | (Var(v1), Var(v2)) => v1 == v2
    | (Var(_), _) => false
    | (Rec(tp1, t1), Rec(tp2, t2)) =>
      TPat.fast_equal(tp1, tp2) && Typ.fast_equal(t1, t2)
    | (Rec(_), _) => false
    | (Parens(t1), Parens(t2)) => Typ.fast_equal(t1, t2)
    | (Parens(_), _) => false
    | (TypHole, TypHole) => true
    | (TypHole, _) => false
    };
}
and TPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = tpat_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = tpat_t;

  let map_term: (~f_tpat: (tpat_t => tpat_t, tpat_t) => tpat_t=?, t) => t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = tpat_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = tpat_t;

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

  let fast_equal = (x: t, y: t) =>
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Hole(_), _) => false
    | (Var(v1), Var(v2)) => v1 == v2
    | (Var(_), _) => false
    };
};
