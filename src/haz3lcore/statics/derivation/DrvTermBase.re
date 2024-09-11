// ALFA logic
open Util;

let continue = Fun.id;
let stop = snd;

module rec Jdmt: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid
    | Val(ALFA_Exp.t)
    | Eval(ALFA_Exp.t, ALFA_Exp.t)
    | Entail(Prop.t, Prop.t)
  and t = IdTagged.t(term);

  let map_term:
    (
      ~f_jdmt: (Jdmt.t => Jdmt.t, Jdmt.t) => Jdmt.t=?,
      ~f_prop: (Prop.t => Prop.t, Prop.t) => Prop.t=?,
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
    | Invalid
    | Val(ALFA_Exp.t)
    | Eval(ALFA_Exp.t, ALFA_Exp.t)
    | Entail(Prop.t, Prop.t)
  and t = IdTagged.t(term);

  let map_term =
      (
        ~f_jdmt=continue,
        ~f_prop=continue,
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        x,
      ) => {
    let prop_map_term =
      Prop.map_term(~f_prop, ~f_exp, ~f_pat, ~f_typ, ~f_tpat);
    let exp_map_term = ALFA_Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Invalid => Invalid
        | Val(p) => Val(exp_map_term(p))
        | Eval(p1, p2) => Eval(exp_map_term(p1), exp_map_term(p2))
        | Entail(p1, p2) => Entail(prop_map_term(p1), prop_map_term(p2))
        },
    };
    x |> f_jdmt(rec_call);
  };

  let fast_equal = (x, y) =>
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Invalid, _) => false
    | (Val(p1), Val(p2)) => ALFA_Exp.fast_equal(p1, p2)
    | (Val(_), _) => false
    | (Eval(p1, p2), Eval(p1', p2')) =>
      ALFA_Exp.fast_equal(p1, p1') && ALFA_Exp.fast_equal(p2, p2')
    | (Eval(_, _), _) => false
    | (Entail(p1, p2), Entail(p1', p2')) =>
      Prop.fast_equal(p1, p1') && Prop.fast_equal(p2, p2')
    | (Entail(_, _), _) => false
    };
}
and Prop: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid
    | HasTy(ALFA_Exp.t, ALFA_Typ.t)
    | Syn(ALFA_Exp.t, ALFA_Typ.t)
    | Ana(ALFA_Exp.t, ALFA_Typ.t)
    | Var(Var.t)
    | And(t, t)
    | Or(t, t)
    | Impl(t, t)
    | Truth
    | Falsity
    | Cons(t, t)
    | Nil
    | Parens(t)
  and t = IdTagged.t(term);

  let map_term:
    (
      ~f_prop: (Prop.t => Prop.t, Prop.t) => Prop.t=?,
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
    | Invalid
    | HasTy(ALFA_Exp.t, ALFA_Typ.t)
    | Syn(ALFA_Exp.t, ALFA_Typ.t)
    | Ana(ALFA_Exp.t, ALFA_Typ.t)
    | Var(Var.t)
    | And(t, t)
    | Or(t, t)
    | Impl(t, t)
    | Truth
    | Falsity
    | Cons(t, t)
    | Nil
    | Parens(t)
  and t = IdTagged.t(term);

  let map_term =
      (
        ~f_prop=continue,
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        x,
      ) => {
    let exp_map_term = ALFA_Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat);
    let typ_map_term = ALFA_Typ.map_term(~f_typ, ~f_tpat);
    let prop_map_term =
      Prop.map_term(~f_prop, ~f_exp, ~f_pat, ~f_typ, ~f_tpat);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Invalid => Invalid
        | HasTy(e, t) => HasTy(exp_map_term(e), typ_map_term(t))
        | Syn(e, t) => Syn(exp_map_term(e), typ_map_term(t))
        | Ana(e, t) => Ana(exp_map_term(e), typ_map_term(t))
        | Var(v) => Var(v)
        | And(p1, p2) => And(prop_map_term(p1), prop_map_term(p2))
        | Or(p1, p2) => Or(prop_map_term(p1), prop_map_term(p2))
        | Impl(p1, p2) => Impl(prop_map_term(p1), prop_map_term(p2))
        | Truth => Truth
        | Falsity => Falsity
        | Cons(p1, p2) => Cons(prop_map_term(p1), prop_map_term(p2))
        | Nil => Nil
        | Parens(p) => Parens(prop_map_term(p))
        },
    };
    x |> f_prop(rec_call);
  };

  let fast_equal = (x, y) =>
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Invalid, _) => false
    | (HasTy(e1, t1), HasTy(e2, t2)) =>
      ALFA_Exp.fast_equal(e1, e2) && ALFA_Typ.fast_equal(t1, t2)
    | (HasTy(_), _) => false
    | (Syn(e1, t1), Syn(e2, t2)) =>
      ALFA_Exp.fast_equal(e1, e2) && ALFA_Typ.fast_equal(t1, t2)
    | (Syn(_), _) => false
    | (Ana(e1, t1), Ana(e2, t2)) =>
      ALFA_Exp.fast_equal(e1, e2) && ALFA_Typ.fast_equal(t1, t2)
    | (Ana(_), _) => false
    | (Var(v1), Var(v2)) => v1 == v2
    | (Var(_), _) => false
    | (And(p1, p2), And(p1', p2')) =>
      Prop.fast_equal(p1, p1') && Prop.fast_equal(p2, p2')
    | (And(_), _) => false
    | (Or(p1, p2), Or(p1', p2')) =>
      Prop.fast_equal(p1, p1') && Prop.fast_equal(p2, p2')
    | (Or(_), _) => false
    | (Impl(p1, p2), Impl(p1', p2')) =>
      Prop.fast_equal(p1, p1') && Prop.fast_equal(p2, p2')
    | (Impl(_), _) => false
    | (Truth, Truth)
    | (Truth, _) => false
    | (Falsity, Falsity)
    | (Falsity, _) => false
    | (Cons(p1, p2), Cons(p1', p2')) =>
      Prop.fast_equal(p1, p1') && Prop.fast_equal(p2, p2')
    | (Cons(_), _) => false
    | (Nil, Nil)
    | (Nil, _) => false
    | (Parens(p1), Parens(p2)) => Prop.fast_equal(p1, p2)
    | (Parens(_), _) => false
    };
}
and ALFA_Exp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type unop =
    | OpNeg;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type binop =
    | OpLt
    | OpGt
    | OpEq
    | OpPlus
    | OpMinus
    | OpTimes;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid
    | NumLit(int)
    | UnOp(unop, t)
    | BinOp(binop, t, t)
    | True
    | False
    | If(t, t, t)
    | Var(Var.t)
    | Let(ALFA_Pat.t, t, t)
    | Fix(ALFA_Pat.t, t)
    | Fun(ALFA_Pat.t, t)
    | Ap(t, t)
    | Pair(t, t)
    | Triv
    | PrjL(t)
    | PrjR(t)
    | InjL
    | InjR
    | Case(t, ALFA_Pat.t, t, ALFA_Pat.t, t)
    | Roll
    | Unroll
    | Parens(t)
  and t = IdTagged.t(term);

  let map_term:
    (
      ~f_exp: (ALFA_Exp.t => ALFA_Exp.t, ALFA_Exp.t) => ALFA_Exp.t=?,
      ~f_pat: (ALFA_Pat.t => ALFA_Pat.t, ALFA_Pat.t) => ALFA_Pat.t=?,
      ~f_typ: (ALFA_Typ.t => ALFA_Typ.t, ALFA_Typ.t) => ALFA_Typ.t=?,
      ~f_tpat: (ALFA_TPat.t => ALFA_TPat.t, ALFA_TPat.t) => ALFA_TPat.t=?,
      t
    ) =>
    t;

  let subst: (t, Var.t, t) => t;

  let subst_ty: (ALFA_Typ.t, Var.t, t) => t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type unop =
    | OpNeg;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type binop =
    | OpLt
    | OpGt
    | OpEq
    | OpPlus
    | OpMinus
    | OpTimes;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid
    | NumLit(int)
    | UnOp(unop, t)
    | BinOp(binop, t, t)
    | True
    | False
    | If(t, t, t)
    | Var(Var.t)
    | Let(ALFA_Pat.t, t, t)
    | Fix(ALFA_Pat.t, t)
    | Fun(ALFA_Pat.t, t)
    | Ap(t, t)
    | Pair(t, t)
    | Triv
    | PrjL(t)
    | PrjR(t)
    | InjL
    | InjR
    | Case(t, ALFA_Pat.t, t, ALFA_Pat.t, t)
    | Roll
    | Unroll
    | Parens(t)
  and t = IdTagged.t(term);

  let map_term =
      (~f_exp=continue, ~f_pat=continue, ~f_typ=continue, ~f_tpat=continue, x) => {
    let exp_map_term = ALFA_Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat);
    let pat_map_term = ALFA_Pat.map_term(~f_pat, ~f_typ, ~f_tpat);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Invalid => Invalid
        | NumLit(n) => NumLit(n)
        | UnOp(op, e) => UnOp(op, exp_map_term(e))
        | BinOp(op, e1, e2) =>
          BinOp(op, exp_map_term(e1), exp_map_term(e2))
        | True => True
        | False => False
        | If(e1, e2, e3) =>
          If(exp_map_term(e1), exp_map_term(e2), exp_map_term(e3))
        | Var(v) => Var(v)
        | Let(p, e1, e2) =>
          Let(pat_map_term(p), exp_map_term(e1), exp_map_term(e2))
        | Fix(p, e) => Fix(pat_map_term(p), exp_map_term(e))
        | Fun(p, e) => Fun(pat_map_term(p), exp_map_term(e))
        | Ap(e1, e2) => Ap(exp_map_term(e1), exp_map_term(e2))
        | Pair(e1, e2) => Pair(exp_map_term(e1), exp_map_term(e2))
        | Triv => Triv
        | PrjL(e) => PrjL(exp_map_term(e))
        | PrjR(e) => PrjR(exp_map_term(e))
        | InjL => InjL
        | InjR => InjR
        | Case(e, p1, e1, p2, e2) =>
          Case(
            exp_map_term(e),
            pat_map_term(p1),
            exp_map_term(e1),
            pat_map_term(p2),
            exp_map_term(e2),
          )
        | Roll => Roll
        | Unroll => Unroll
        | Parens(e) => Parens(exp_map_term(e))
        },
    };
    x |> f_exp(rec_call);
  };

  let rec subst = (v, x, e) => {
    let (term, rewrap) = IdTagged.unwrap(e);
    let subst = subst(v, x);
    let is_shadowed = p => ALFA_Pat.var_of_pat(p) == Some(x);
    let subx = p => is_shadowed(p) ? Fun.id : subst;
    switch (term) {
    | Invalid => e
    | NumLit(_) => e
    | UnOp(op, e) => UnOp(op, subst(e)) |> rewrap
    | BinOp(op, e1, e2) => BinOp(op, subst(e1), subst(e2)) |> rewrap
    | True => e
    | False => e
    | If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3)) |> rewrap
    | Var(x') => x' == x ? v : e
    | Let(p, e1, e2) => Let(p, subst(e1), subx(p, e2)) |> rewrap
    | Fix(p, e) => Fix(p, subx(p, e)) |> rewrap
    | Fun(p, e) => Fun(p, subx(p, e)) |> rewrap
    | Ap(e1, e2) => Ap(subst(e1), subst(e2)) |> rewrap
    | Pair(e1, e2) => Pair(subst(e1), subst(e2)) |> rewrap
    | Triv => e
    | PrjL(e) => PrjL(subst(e)) |> rewrap
    | PrjR(e) => PrjR(subst(e)) |> rewrap
    | InjL => e
    | InjR => e
    | Case(e, p1, e1, p2, e2) =>
      Case(subst(e), p1, subx(p1, e1), p2, subx(p2, e2)) |> rewrap
    | Roll => e
    | Unroll => e
    | Parens(e) => Parens(subst(e)) |> rewrap
    };
  };

  let rec subst_ty = (t, x, e) => {
    let (term, rewrap) = IdTagged.unwrap(e);
    let subst_ty = subst_ty(t, x);
    let pat_subst_ty = ALFA_Pat.subst_ty(t, x);
    switch (term) {
    | Invalid => e
    | NumLit(_) => e
    | UnOp(op, e) => UnOp(op, subst_ty(e)) |> rewrap
    | BinOp(op, e1, e2) => BinOp(op, subst_ty(e1), subst_ty(e2)) |> rewrap
    | True => e
    | False => e
    | If(e1, e2, e3) =>
      If(subst_ty(e1), subst_ty(e2), subst_ty(e3)) |> rewrap
    | Var(_) => e
    | Let(p, e1, e2) =>
      Let(pat_subst_ty(p), subst_ty(e1), subst_ty(e2)) |> rewrap
    | Fix(p, e) => Fix(pat_subst_ty(p), subst_ty(e)) |> rewrap
    | Fun(p, e) => Fun(pat_subst_ty(p), subst_ty(e)) |> rewrap
    | Ap(e1, e2) => Ap(subst_ty(e1), subst_ty(e2)) |> rewrap
    | Pair(e1, e2) => Pair(subst_ty(e1), subst_ty(e2)) |> rewrap
    | Triv => e
    | PrjL(e) => PrjL(subst_ty(e)) |> rewrap
    | PrjR(e) => PrjR(subst_ty(e)) |> rewrap
    | InjL => e
    | InjR => e
    | Case(e, p1, e1, p2, e2) =>
      Case(
        subst_ty(e),
        pat_subst_ty(p1),
        subst_ty(e1),
        pat_subst_ty(p2),
        subst_ty(e2),
      )
      |> rewrap
    | Roll => e
    | Unroll => e
    | Parens(e) => Parens(subst_ty(e)) |> rewrap
    };
  };

  let fast_equal = (x, y) =>
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Invalid, _) => false
    | (NumLit(n1), NumLit(n2)) => n1 == n2
    | (NumLit(_), _) => false
    | (UnOp(op1, e1), UnOp(op2, e2)) =>
      op1 == op2 && ALFA_Exp.fast_equal(e1, e2)
    | (UnOp(_), _) => false
    | (BinOp(op1, e11, e12), BinOp(op2, e21, e22)) =>
      op1 == op2
      && ALFA_Exp.fast_equal(e11, e21)
      && ALFA_Exp.fast_equal(e12, e22)
    | (BinOp(_), _) => false
    | (True, True) => true
    | (True, _) => false
    | (False, False) => true
    | (False, _) => false
    | (If(e1, e2, e3), If(e1', e2', e3')) =>
      ALFA_Exp.fast_equal(e1, e1')
      && ALFA_Exp.fast_equal(e2, e2')
      && ALFA_Exp.fast_equal(e3, e3')
    | (If(_), _) => false
    | (Var(v1), Var(v2)) => v1 == v2
    | (Var(_), _) => false
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
    | (Pair(e11, e12), Pair(e21, e22)) =>
      ALFA_Exp.fast_equal(e11, e21) && ALFA_Exp.fast_equal(e12, e22)
    | (Pair(_), _) => false
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
    | (Case(e1, p1, e11, p2, e12), Case(e2, p3, e21, p4, e22)) =>
      ALFA_Exp.fast_equal(e1, e2)
      && ALFA_Pat.fast_equal(p1, p3)
      && ALFA_Exp.fast_equal(e11, e21)
      && ALFA_Pat.fast_equal(p2, p4)
      && ALFA_Exp.fast_equal(e12, e22)
    | (Case(_), _) => false
    | (Roll, Roll) => true
    | (Roll, _) => false
    | (Unroll, Unroll) => true
    | (Unroll, _) => false
    | (Parens(e1), Parens(e2)) => ALFA_Exp.fast_equal(e1, e2)
    | (Parens(_), _) => false
    };
}
and ALFA_Pat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid
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
      ~f_pat: (ALFA_Pat.t => ALFA_Pat.t, ALFA_Pat.t) => ALFA_Pat.t=?,
      ~f_typ: (ALFA_Typ.t => ALFA_Typ.t, ALFA_Typ.t) => ALFA_Typ.t=?,
      ~f_tpat: (ALFA_TPat.t => ALFA_TPat.t, ALFA_TPat.t) => ALFA_TPat.t=?,
      t
    ) =>
    t;

  let var_of_pat: t => option(Var.t);

  let subst_ty: (ALFA_Typ.t, Var.t, t) => t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid
    | Var(Var.t)
    | Cast(t, ALFA_Typ.t)
    | InjL
    | InjR
    | Ap(t, t)
    | Pair(t, t)
    | Parens(t)
  and t = IdTagged.t(term);

  let map_term = (~f_pat=continue, ~f_typ=continue, ~f_tpat=continue, x) => {
    let pat_map_term = ALFA_Pat.map_term(~f_pat, ~f_typ, ~f_tpat);
    let typ_map_term = ALFA_Typ.map_term(~f_typ, ~f_tpat);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Invalid => Invalid
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

  let var_of_pat = x =>
    switch (x |> IdTagged.term_of) {
    | Invalid => None
    | Var(v) => Some(v)
    | Cast(p, _) => ALFA_Pat.var_of_pat(p)
    | InjL => None
    | InjR => None
    | Ap(_) => None
    | Pair(_) => None
    | Parens(p) => ALFA_Pat.var_of_pat(p)
    };

  let rec subst_ty = (t, x, p) => {
    let (term, rewrap) = IdTagged.unwrap(p);
    let subst_ty = subst_ty(t, x);
    switch (term) {
    | Invalid => p
    | Var(_) => p
    | Cast(p, t') =>
      Cast(subst_ty(p), ALFA_Typ.subst_ty(t, x, t')) |> rewrap
    | InjL => p
    | InjR => p
    | Ap(t1, t2) => Ap(subst_ty(t1), subst_ty(t2)) |> rewrap
    | Pair(t1, t2) => Pair(subst_ty(t1), subst_ty(t2)) |> rewrap
    | Parens(p) => Parens(subst_ty(p)) |> rewrap
    };
  };

  let fast_equal = (x, y) =>
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Invalid, _) => false
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
    | Invalid
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
      ~f_typ: (ALFA_Typ.t => ALFA_Typ.t, ALFA_Typ.t) => ALFA_Typ.t=?,
      ~f_tpat: (ALFA_TPat.t => ALFA_TPat.t, ALFA_TPat.t) => ALFA_TPat.t=?,
      t
    ) =>
    t;

  let subst_ty: (ALFA_Typ.t, Var.t, t) => t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid
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

  let map_term = (~f_typ=continue, ~f_tpat=continue, x) => {
    let typ_map_term = ALFA_Typ.map_term(~f_typ, ~f_tpat);
    let tpat_map_term = ALFA_TPat.map_term(~f_tpat);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Invalid => Invalid
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

  let rec subst_ty = (t, x, e) => {
    let (term, rewrap) = IdTagged.unwrap(e);
    let subst_ty = subst_ty(t, x);
    let is_shadowed = p => ALFA_TPat.var_of_tpat(p) == Some(x);
    let subx = p => is_shadowed(p) ? Fun.id : subst_ty;
    switch (term) {
    | Invalid => e
    | Num => e
    | Bool => e
    | Arrow(t1, t2) => Arrow(subst_ty(t1), subst_ty(t2)) |> rewrap
    | Prod(t1, t2) => Prod(subst_ty(t1), subst_ty(t2)) |> rewrap
    | Unit => e
    | Sum(t1, t2) => Sum(subst_ty(t1), subst_ty(t2)) |> rewrap
    | Var(x') => x' == x ? t : e
    | Rec(tp, t) => Rec(tp, subx(tp, t)) |> rewrap
    | Parens(t) => Parens(subst_ty(t)) |> rewrap
    };
  };

  let fast_equal = (x, y) =>
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Invalid, _) => false
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
    | Invalid
    | Var(Var.t)
  and t = IdTagged.t(term);

  let map_term:
    (~f_tpat: (ALFA_TPat.t => ALFA_TPat.t, ALFA_TPat.t) => ALFA_TPat.t=?, t) =>
    t;

  let var_of_tpat: t => option(Var.t);

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term =
    | Invalid
    | Var(Var.t)
  and t = IdTagged.t(term);

  let map_term = (~f_tpat=continue, x) => {
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Invalid => Invalid
        | Var(v) => Var(v)
        },
    };
    x |> f_tpat(rec_call);
  };

  let var_of_tpat = x =>
    switch (x |> IdTagged.term_of) {
    | Invalid => None
    | Var(v) => Some(v)
    };

  let fast_equal = (x, y) =>
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Invalid, _) => false
    | (Var(v1), Var(v2)) => v1 == v2
    | (Var(_), _) => false
    };
};
