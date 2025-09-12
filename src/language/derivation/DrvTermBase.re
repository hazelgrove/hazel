let continue = x => x;
let stop = (_, x) => x;

[@deriving (show({with_path: false}), sexp, yojson)]
type any_t = DrvGrammar.any_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type exp_t = DrvGrammar.exp_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type exp_term = DrvGrammar.exp_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type pat_t = DrvGrammar.pat_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type pat_term = DrvGrammar.pat_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type typ_t = DrvGrammar.typ_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type typ_term = DrvGrammar.typ_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type tpat_t = DrvGrammar.tpat_t(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type tpat_term = DrvGrammar.tpat_term(IdTagged.IdTag.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type type_hole = DrvGrammar.type_hole(IdTagged.IdTag.t);

module rec Any: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = any_t;

  let map_term:
    (
      ~f_exp: (exp_t => exp_t, exp_t) => exp_t=?,
      ~f_pat: (pat_t => pat_t, pat_t) => pat_t=?,
      ~f_typ: (typ_t => typ_t, typ_t) => typ_t=?,
      ~f_tpat: (tpat_t => tpat_t, tpat_t) => tpat_t=?,
      ~f_any: (any_t => any_t, any_t) => any_t=?,
      t
    ) =>
    t;

  let eq: (t, t, ~skip_hole: bool) => bool;

  let sort: t => DrvSort.t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = any_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_any=continue,
        x: t,
      ) => {
    let rec_call = (y: t): t =>
      switch (y) {
      | Exp(x) =>
        Exp(Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_any, x))
      | Pat(x) =>
        Pat(Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_any, x))
      | Typ(x) =>
        Typ(Typ.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_any, x))
      | TPat(x) =>
        TPat(TPat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_any, x))
      };
    x |> f_any(rec_call);
  };

  let eq = (x: t, y: t, ~skip_hole: bool) =>
    switch (x, y) {
    | (Exp(e1), Exp(e2)) => Exp.eq(e1, e2, ~skip_hole)
    | (Exp(_), _) => false
    | (Pat(p1), Pat(p2)) => Pat.eq(p1, p2, ~skip_hole)
    | (Pat(_), _) => false
    | (Typ(t1), Typ(t2)) => Typ.eq(t1, t2, ~skip_hole)
    | (Typ(_), _) => false
    | (TPat(tp1), TPat(tp2)) => TPat.eq(tp1, tp2, ~skip_hole)
    | (TPat(_), _) => false
    };

  let sort = (any: t): DrvSort.t =>
    switch (any) {
    | Exp(_) => Exp
    | Pat(_) => Pat
    | Typ(_) => Typ
    | TPat(_) => TPat
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
      ~f_any: (any_t => any_t, any_t) => any_t=?,
      t
    ) =>
    t;

  let subst: (t, string, t) => t;

  let eq: (t, t, ~skip_hole: bool) => bool;

  let mem_ctx: (t, list(t)) => bool;

  let subset_ctx: (list(t), list(t)) => bool;

  let cons_ctx: (list(t), t) => list(t);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = exp_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = exp_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_any=continue,
        x: t,
      ) => {
    let exp_map_term = Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_any);
    let pat_map_term = Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_any);
    let typ_map_term = Typ.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_any);
    let any_map_term = Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_any);
    let rec_call = ({term, _} as exp: t): t => {
      ...exp,
      term:
        switch (term) {
        | Hole(MultiHole(l)) => Hole(MultiHole(List.map(any_map_term, l)))
        | Hole(_) => term
        | Var(v) => Var(v)
        | Quote(s) => Quote(s)
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
        | BinOp(op, e1, e2) =>
          BinOp(op, exp_map_term(e1), exp_map_term(e2))
        | True => True
        | False => False
        | If(e1, e2, e3) =>
          If(exp_map_term(e1), exp_map_term(e2), exp_map_term(e3))
        | Let(p, e1, e2) =>
          Let(pat_map_term(p), exp_map_term(e1), exp_map_term(e2))
        | Fix(p, e) => Fix(pat_map_term(p), exp_map_term(e))
        | Fun(p, e) => Fun(pat_map_term(p), exp_map_term(e))
        | Ap(e1, e2) => Ap(exp_map_term(e1), exp_map_term(e2))
        | Pair(e1, e2) => Pair(exp_map_term(e1), exp_map_term(e2))
        | Triv => Triv
        | PrjL(e) => PrjL(exp_map_term(e))
        | PrjR(e) => PrjR(exp_map_term(e))
        | InjL(e) => InjL(exp_map_term(e))
        | InjR(e) => InjR(exp_map_term(e))
        | Case(e, p1, e1, p2, e2) =>
          Case(
            exp_map_term(e),
            pat_map_term(p1),
            exp_map_term(e1),
            pat_map_term(p2),
            exp_map_term(e2),
          )
        | Roll(e) => Roll(exp_map_term(e))
        | Unroll(e) => Unroll(exp_map_term(e))
        | ExpHole => ExpHole
        },
    };
    x |> f_exp(rec_call);
  };

  let rec subst = (v: t, x: string, e: t) => {
    let (term, rewrap: term => t) = IdTagged.unwrap(e);
    let subst = subst(v, x);
    let rec is_shadow = (p: pat_t) =>
      switch (IdTagged.term_of(p)) {
      | Var(x') => String.equal(x', x)
      | Cast(p, _) => is_shadow(p)
      | Pair(p1, p2) => is_shadow(p1) || is_shadow(p2)
      | _ => false
      };
    let subst' = p => is_shadow(p) ? Fun.id : subst;
    switch (term) {
    | Hole(_) => e
    | Var(x') => String.equal(x', x) ? v : e
    | Quote(_) => e
    | Parens(e) => Parens(subst(e)) |> rewrap
    | Tuple(es) => Tuple(List.map(subst, es)) |> rewrap
    // Jdmt
    | Val(_)
    | Eval(_)
    | Entail(_)
    | Consistent(_)
    | MatchedArrow(_)
    | MatchedProd(_)
    | MatchedSum(_) => e
    // Ctx
    | Ctx(_)
    | Cons(_)
    | Concat(_) => e
    // Prop
    | Type(_)
    | HasType(_)
    | Syn(_)
    | Ana(_)
    | And(_)
    | Or(_)
    | Impl(_)
    | Truth
    | Falsity => e
    // Typ
    | NumLit(_) => e
    | Neg(e) => Neg(subst(e)) |> rewrap
    | BinOp(op, e1, e2) => BinOp(op, subst(e1), subst(e2)) |> rewrap
    | True
    | False => e
    | If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3)) |> rewrap
    | Let(x, e1, e2) => Let(x, subst(e1), subst'(x, e2)) |> rewrap
    | Fix(x, e) => Fix(x, subst'(x, e)) |> rewrap
    | Fun(x, e) => Fun(x, subst'(x, e)) |> rewrap
    | Ap(e1, e2) => Ap(subst(e1), subst(e2)) |> rewrap
    | Pair(e1, e2) => Pair(subst(e1), subst(e2)) |> rewrap
    | Triv => e
    | PrjL(e) => PrjL(subst(e)) |> rewrap
    | PrjR(e) => PrjR(subst(e)) |> rewrap
    | InjL(e) => InjL(subst(e)) |> rewrap
    | InjR(e) => InjR(subst(e)) |> rewrap
    | Case(e, x, e1, y, e2) =>
      Case(subst(e), x, subst'(x, e1), y, subst'(y, e2)) |> rewrap
    | Roll(e) => Roll(subst(e)) |> rewrap
    | Unroll(e) => Unroll(subst(e)) |> rewrap
    | ExpHole => e
    };
  };

  let rec eq = (x: t, y: t, ~skip_hole: bool) => {
    let eq = eq(~skip_hole);
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Hole(_), _)
    | (_, Hole(_)) when skip_hole => true
    // Note(zhiyao): This is to avoid infinite loop in cell update
    | (Hole(_), _) => x == y
    | (Var(v1), Var(v2)) => v1 == v2
    | (Var(_), _) => false
    | (Quote(s1), Quote(s2)) => s1 == s2
    | (Quote(_), _) => false
    | (Parens(e1), Parens(e2)) => eq(e1, e2)
    | (Parens(_), _) => false
    | (Val(e1), Val(e2)) => eq(e1, e2)
    | (Val(_), _) => false
    | (Eval(e11, e12), Eval(e21, e22)) => eq(e11, e21) && eq(e12, e22)
    | (Eval(_), _) => false
    | (Entail(e11, e12), Entail(e21, e22)) => eq(e11, e21) && eq(e12, e22)
    | (Entail(_), _) => false
    | (Consistent(t11, t12), Consistent(t21, t22)) =>
      Typ.eq(t11, t21, ~skip_hole) && Typ.eq(t12, t22, ~skip_hole)
    | (Consistent(_), _) => false
    | (MatchedArrow(t11, t12), MatchedArrow(t21, t22)) =>
      Typ.eq(t11, t21, ~skip_hole) && Typ.eq(t12, t22, ~skip_hole)
    | (MatchedArrow(_), _) => false
    | (MatchedProd(t11, t12), MatchedProd(t21, t22)) =>
      Typ.eq(t11, t21, ~skip_hole) && Typ.eq(t12, t22, ~skip_hole)
    | (MatchedProd(_), _) => false
    | (MatchedSum(t11, t12), MatchedSum(t21, t22)) =>
      Typ.eq(t11, t21, ~skip_hole) && Typ.eq(t12, t22, ~skip_hole)
    | (MatchedSum(_), _) => false
    | (Ctx(es1), Ctx(es2)) =>
      List.length(es1) == List.length(es2) && List.for_all2(eq, es1, es2)
    | (Ctx(_), _) => false
    | (Cons(e11, e12), Cons(e21, e22)) => eq(e11, e21) && eq(e12, e22)
    | (Cons(_), _) => false
    | (Concat(e11, e12), Concat(e21, e22)) => eq(e11, e21) && eq(e12, e22)
    | (Concat(_), _) => false
    | (Type(t1), Type(t2)) => Typ.eq(t1, t2, ~skip_hole)
    | (Type(_), _) => false
    | (HasType(e1, t1), HasType(e2, t2)) =>
      eq(e1, e2) && Typ.eq(t1, t2, ~skip_hole)
    | (HasType(_), _) => false
    | (Syn(e1, t1), Syn(e2, t2)) =>
      eq(e1, e2) && Typ.eq(t1, t2, ~skip_hole)
    | (Syn(_), _) => false
    | (Ana(e1, t1), Ana(e2, t2)) =>
      eq(e1, e2) && Typ.eq(t1, t2, ~skip_hole)
    | (Ana(_), _) => false
    | (And(e11, e12), And(e21, e22)) => eq(e11, e21) && eq(e12, e22)
    | (And(_), _) => false
    | (Or(e11, e12), Or(e21, e22)) => eq(e11, e21) && eq(e12, e22)
    | (Or(_), _) => false
    | (Impl(e11, e12), Impl(e21, e22)) => eq(e11, e21) && eq(e12, e22)
    | (Impl(_), _) => false
    | (Truth, Truth) => true
    | (Truth, _) => false
    | (Falsity, Falsity) => true
    | (Falsity, _) => false
    | (Tuple(es1), Tuple(es2)) =>
      List.length(es1) == List.length(es2) && List.for_all2(eq, es1, es2)
    | (Tuple(_), _) => false
    | (NumLit(n1), NumLit(n2)) => n1 == n2
    | (NumLit(_), _) => false
    | (Neg(e1), Neg(e2)) => eq(e1, e2)
    | (Neg(_), _) => false
    | (BinOp(op1, e11, e12), BinOp(op2, e21, e22)) =>
      op1 == op2 && eq(e11, e21) && eq(e12, e22)
    | (BinOp(_), _) => false
    | (True, True) => true
    | (True, _) => false
    | (False, False) => true
    | (False, _) => false
    | (If(e1, e2, e3), If(e1', e2', e3')) =>
      eq(e1, e1') && eq(e2, e2') && eq(e3, e3')
    | (If(_), _) => false
    | (Let(p1, e11, e12), Let(p2, e21, e22)) =>
      Pat.eq(p1, p2, ~skip_hole) && eq(e11, e21) && eq(e12, e22)
    | (Let(_), _) => false
    | (Fix(p1, e1), Fix(p2, e2)) =>
      Pat.eq(p1, p2, ~skip_hole) && eq(e1, e2)
    | (Fix(_), _) => false
    | (Fun(p1, e1), Fun(p2, e2)) =>
      Pat.eq(p1, p2, ~skip_hole) && eq(e1, e2)
    | (Fun(_), _) => false
    | (Ap(e11, e12), Ap(e21, e22)) => eq(e11, e21) && eq(e12, e22)
    | (Ap(_), _) => false
    | (Pair(e11, e12), Pair(e21, e22)) => eq(e11, e21) && eq(e12, e22)
    | (Pair(_), _) => false
    | (Triv, Triv) => true
    | (Triv, _) => false
    | (PrjL(e1), PrjL(e2)) => eq(e1, e2)
    | (PrjL(_), _) => false
    | (PrjR(e1), PrjR(e2)) => eq(e1, e2)
    | (PrjR(_), _) => false
    | (InjL(e1), InjL(e2)) => eq(e1, e2)
    | (InjL(_), _) => false
    | (InjR(e1), InjR(e2)) => eq(e1, e2)
    | (InjR(_), _) => false
    | (Case(e1, x1, e11, y1, e12), Case(e2, x2, e21, y2, e22)) =>
      eq(e1, e2)
      && Pat.eq(x1, x2, ~skip_hole)
      && eq(e11, e21)
      && Pat.eq(y1, y2, ~skip_hole)
      && eq(e12, e22)
    | (Case(_), _) => false
    | (Roll(e1), Roll(e2)) => eq(e1, e2)
    | (Roll(_), _) => false
    | (Unroll(e1), Unroll(e2)) => eq(e1, e2)
    | (Unroll(_), _) => false
    | (ExpHole, ExpHole) => true
    | (ExpHole, _) => false
    };
  };

  let rec splice_on_exist = (p, l) =>
    switch (l) {
    | [] => []
    | [hd, ...tl] =>
      eq(p, hd, ~skip_hole=false) ? l : splice_on_exist(p, tl)
    };

  let mem_ctx = (p, l) => splice_on_exist(p, l) != [];

  let rec subset_ctx = (s, l) =>
    switch (s, l) {
    | ([], _) => true
    | (_, []) => false
    | ([hd, ...tl], l) =>
      switch (splice_on_exist(hd, l)) {
      | [] => false
      | [_, ...tl'] => subset_ctx(tl, tl')
      }
    };

  // Note(zhiyao): This implementation of cons_ctx is not linear.
  let cons_ctx = (ctx, p) => {
    let cmp = p' => show(p) < show(p');
    let eq = eq(~skip_hole=false);
    let eq_key = p' =>
      switch (IdTagged.term_of(p): term, IdTagged.term_of(p'): term) {
      | (HasType(a, _), HasType(b, _)) => eq(a, b)
      | (Syn(a, _), Syn(b, _)) => eq(a, b)
      | (Ana(a, _), Ana(b, _)) => eq(a, b)
      | _ => show(p) == show(p')
      };
    let rec insert =
      fun
      | [] => [p]
      | [hd, ...tl] when eq_key(hd) => [p, ...tl]
      | [hd, ...tl] when cmp(hd) => [p, hd, ...tl]
      | [hd, ...tl] => [hd, ...insert(tl)];
    insert(ctx);
  };
}
and Pat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = pat_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = pat_t;

  let map_term:
    (
      ~f_exp: (exp_t => exp_t, exp_t) => exp_t=?,
      ~f_pat: (pat_t => pat_t, pat_t) => pat_t=?,
      ~f_typ: (typ_t => typ_t, typ_t) => typ_t=?,
      ~f_tpat: (tpat_t => tpat_t, tpat_t) => tpat_t=?,
      ~f_any: (any_t => any_t, any_t) => any_t=?,
      t
    ) =>
    t;

  let eq: (t, t, ~skip_hole: bool) => bool;
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
        ~f_any=continue,
        x: t,
      ) => {
    let pat_map_term = Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_any);
    let typ_map_term = Typ.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_any);
    let any_map_term = Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_any);
    let rec_call = ({term, _} as exp: t): t => {
      ...exp,
      term:
        switch (term) {
        | Hole(MultiHole(l)) => Hole(MultiHole(List.map(any_map_term, l)))
        | Hole(_) => term
        | Quote(s) => Quote(s)
        | Var(v) => Var(v)
        | Parens(p) => Parens(pat_map_term(p))
        | Cast(p, t) => Cast(pat_map_term(p), typ_map_term(t))
        | InjL(p) => InjL(pat_map_term(p))
        | InjR(p) => InjR(pat_map_term(p))
        | Pair(p1, p2) => Pair(pat_map_term(p1), pat_map_term(p2))
        },
    };
    x |> f_pat(rec_call);
  };

  let rec eq = (x: t, y: t, ~skip_hole: bool) => {
    let eq = eq(~skip_hole);
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Hole(_), _)
    | (_, Hole(_)) when skip_hole => true
    // Note(zhiyao): This is to avoid infinite loop in cell update
    | (Hole(_), _) => x == y
    | (Quote(s1), Quote(s2)) => s1 == s2
    | (Quote(_), _) => false
    | (Var(v1), Var(v2)) => v1 == v2
    | (Var(_), _) => false
    | (Parens(p1), Parens(p2)) => eq(p1, p2)
    | (Parens(_), _) => false
    | (Cast(p1, t1), Cast(p2, t2)) =>
      eq(p1, p2) && Typ.eq(t1, t2, ~skip_hole)
    | (Cast(_), _) => false
    | (InjL(p1), InjL(p2)) => eq(p1, p2)
    | (InjL(_), _) => false
    | (InjR(p1), InjR(p2)) => eq(p1, p2)
    | (InjR(_), _) => false
    | (Pair(p1, p2), Pair(p1', p2')) => eq(p1, p1') && eq(p2, p2')
    | (Pair(_), _) => false
    };
  };
}
and Typ: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = typ_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = typ_t;

  let map_term:
    (
      ~f_exp: (exp_t => exp_t, exp_t) => exp_t=?,
      ~f_pat: (pat_t => pat_t, pat_t) => pat_t=?,
      ~f_typ: (typ_t => typ_t, typ_t) => typ_t=?,
      ~f_tpat: (tpat_t => tpat_t, tpat_t) => tpat_t=?,
      ~f_any: (any_t => any_t, any_t) => any_t=?,
      t
    ) =>
    t;

  let subst: (t, string, t) => t;

  let glb: (t, t) => t;

  let eq: (t, t, ~skip_hole: bool) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = typ_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = typ_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_any=continue,
        x: t,
      ) => {
    let typ_map_term = Typ.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_any);
    let tpat_map_term =
      TPat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_any);
    let any_map_term = Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_any);
    let rec_call = ({term, _} as exp: t): t => {
      ...exp,
      term:
        switch (term) {
        | Hole(MultiHole(l)) => Hole(MultiHole(List.map(any_map_term, l)))
        | Hole(_) => term
        | Quote(s) => Quote(s)
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

  let rec subst = (v: t, x: string, e: t) => {
    let (term, rewrap: term => t) = IdTagged.unwrap(e);
    let subst = subst(v, x);
    let is_shadow = (p: tpat_t) =>
      switch (IdTagged.term_of(p)) {
      | Var(x') => String.equal(x', x)
      | _ => false
      };
    let subst' = p => is_shadow(p) ? Fun.id : subst;
    switch (term) {
    | Hole(_) => e
    | Quote(_) => e
    | Var(x') => String.equal(x', x) ? v : e
    | Num => e
    | Bool => e
    | Arrow(t1, t2) => Arrow(subst(t1), subst(t2)) |> rewrap
    | Prod(t1, t2) => Prod(subst(t1), subst(t2)) |> rewrap
    | Unit => e
    | Sum(t1, t2) => Sum(subst(t1), subst(t2)) |> rewrap
    | Rec(tp, t) => Rec(tp, subst'(tp, t)) |> rewrap
    | Parens(t) => Parens(subst(t)) |> rewrap
    | TypHole => e
    };
  };

  let fresh: term => t = IdTagged.fresh;

  let rec eq = (x: t, y: t, ~skip_hole: bool) => {
    let eq = eq(~skip_hole);
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Hole(_), _)
    | (_, Hole(_)) when skip_hole => true
    // Note(zhiyao): This is to avoid infinite loop in cell update
    | (Hole(_), _) => x == y
    | (Quote(s1), Quote(s2)) => s1 == s2
    | (Quote(_), _) => false
    | (Num, Num) => true
    | (Num, _) => false
    | (Bool, Bool) => true
    | (Bool, _) => false
    | (Arrow(t1, t2), Arrow(t1', t2')) => eq(t1, t1') && eq(t2, t2')
    | (Arrow(_), _) => false
    | (Prod(t1, t2), Prod(t1', t2')) => eq(t1, t1') && eq(t2, t2')
    | (Prod(_), _) => false
    | (Unit, Unit) => true
    | (Unit, _) => false
    | (Sum(t1, t2), Sum(t1', t2')) => eq(t1, t1') && eq(t2, t2')
    | (Sum(_), _) => false
    | (Var(v1), Var(v2)) => v1 == v2
    | (Var(_), _) => false
    | (Rec({term: Var(a1), _}, a2), Rec({term: Var(b1), _}, b2)) =>
      let rep_id = fresh(Var(Id.mk() |> Id.show));
      eq(subst(rep_id, a1, a2), subst(rep_id, b1, b2));
    | (Rec(_), _) => false
    | (Parens(t1), Parens(t2)) => eq(t1, t2)
    | (Parens(_), _) => false
    | (TypHole, TypHole) => true
    | (TypHole, _) => false
    };
  };

  let rec glb = (t1: t, t2: t): t => {
    switch (t1 |> IdTagged.term_of, t2 |> IdTagged.term_of) {
    | _ when eq(t1, t2, ~skip_hole=false) => t1
    | (TypHole, _) => t2
    | (_, TypHole) => t1
    | (Arrow(t11, t12), Arrow(t21, t22)) =>
      Arrow(glb(t11, t21), glb(t12, t22)) |> fresh
    | (Prod(t11, t12), Prod(t21, t22)) =>
      Prod(glb(t11, t21), glb(t12, t22)) |> fresh
    | (Sum(t11, t12), Sum(t21, t22)) =>
      Sum(glb(t11, t21), glb(t12, t22)) |> fresh
    | _ => Hole(Invalid("Glb Failure")) |> fresh
    };
  };
}
and TPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = tpat_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = tpat_t;

  let map_term:
    (
      ~f_exp: (exp_t => exp_t, exp_t) => exp_t=?,
      ~f_pat: (pat_t => pat_t, pat_t) => pat_t=?,
      ~f_typ: (typ_t => typ_t, typ_t) => typ_t=?,
      ~f_tpat: (tpat_t => tpat_t, tpat_t) => tpat_t=?,
      ~f_any: (any_t => any_t, any_t) => any_t=?,
      t
    ) =>
    t;

  let eq: (t, t, ~skip_hole: bool) => bool;
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
        ~f_any=continue,
        x: t,
      ) => {
    let any_map_term = Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_any);
    let rec_call = ({term, _} as exp: t): t => {
      ...exp,
      term:
        switch (term) {
        | Hole(MultiHole(l)) => Hole(MultiHole(List.map(any_map_term, l)))
        | Hole(_) => term
        | Quote(s) => Quote(s)
        | Var(v) => Var(v)
        },
    };
    x |> f_tpat(rec_call);
  };

  let eq = (x: t, y: t, ~skip_hole: bool) => {
    switch (x |> IdTagged.term_of, y |> IdTagged.term_of) {
    | (Hole(_), _)
    | (_, Hole(_)) when skip_hole => true
    // Note(zhiyao): This is to avoid infinite loop in cell update
    | (Hole(_), _) => x == y
    | (Quote(s1), Quote(s2)) => s1 == s2
    | (Quote(_), _) => false
    | (Var(v1), Var(v2)) => v1 == v2
    | (Var(_), _) => false
    };
  };
};
