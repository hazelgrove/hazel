open DrvSyntax;

/**
  This module is responsible for elaborating Drv.Exp.t to DrvSyntax.t.

  It is a shame that I have to duplicate the code though DHExp.t has consolidated
  with Exp.t. But I have to do this because
  1) strict equality is required in `RuleSpec.re` (have to strip casts and parens)
  2) sort separation (not implemented in Drv.Exp.t but needed in DrvSyntax.t)
 */

let to_list = d =>
  switch (DrvSyntax.term_of(d)) {
  | Ctx(ps) => ps
  | _ => [d]
  };

let rec exp_term_of: Drv.Exp.t => Drv.Exp.term =
  exp =>
    switch (exp.term) {
    | Parens(p) => exp_term_of(p)
    | p => p
    }
and elab_jdmt: Drv.Exp.t => t =
  jdmt => {
    let hole: term = Hole(Drv.Exp.show(jdmt));
    let term: term =
      switch (exp_term_of(jdmt)) {
      | Hole(s) => Hole(DrvTermBase.show_type_hole(s))
      | Var(_)
      | Abbr(_)
      | Parens(_)
      | Tuple(_) => hole
      // Jdmt
      | Val(e) => Val(elab_exp(e))
      | Eval(e1, e2) => Eval(elab_exp(e1), elab_exp(e2))
      | Entail(ctx, p) => Entail(elab_ctxt(ctx), elab_prop(p))
      | Consistent(t1, t2) => Consistent(elab_typ(t1), elab_typ(t2))
      | MatchedArrow(t1, t2) => MatchedArrow(elab_typ(t1), elab_typ(t2))
      | MatchedProd(t1, t2) => MatchedProd(elab_typ(t1), elab_typ(t2))
      | MatchedSum(t1, t2) => MatchedSum(elab_typ(t1), elab_typ(t2))
      // Ctx
      | Ctx(_)
      | Cons(_)
      | Concat(_)
      // Prop
      | Type(_)
      | HasType(_)
      | Syn(_)
      | Ana(_)
      | And(_)
      | Or(_)
      | Impl(_)
      | Truth
      | Falsity
      // Exp
      | NumLit(_)
      | Neg(_)
      | Plus(_)
      | Minus(_)
      | Times(_)
      | Gt(_)
      | Lt(_)
      | Eq(_)
      | True
      | False
      | If(_)
      | Let(_)
      | Fix(_)
      | Fun(_)
      | Ap(_)
      | Triv
      | PrjL(_)
      | PrjR(_)
      | InjL(_)
      | InjR(_)
      | Case(_)
      | Roll(_)
      | Unroll(_)
      | ExpHole => hole
      };
    {...jdmt, term};
  }
and elab_ctxt: Drv.Exp.t => t =
  ctx => {
    let hole: term = Hole(Drv.Exp.show(ctx));
    let term: term =
      switch (exp_term_of(ctx)) {
      | Hole(s) => Hole(DrvTermBase.show_type_hole(s))
      | Var(_)
      | Abbr(_)
      | Parens(_)
      | Tuple(_)
      // Jdmt
      | Val(_)
      | Eval(_)
      | Entail(_)
      | Consistent(_)
      | MatchedArrow(_)
      | MatchedProd(_)
      | MatchedSum(_) => hole
      // Ctx
      | Ctx(ps) =>
        Ctx(
          ps
          |> List.map(elab_prop)
          |> List.map(to_list)
          |> List.concat
          |> List.fold_left(cons_ctx, []),
        )
      | Cons(p, ctx) =>
        switch (IdTagged.term_of(elab_ctxt(ctx))) {
        | Ctx(ps) => Ctx(cons_ctx(ps, elab_prop(p)))
        | _ => hole
        }
      | Concat(ctx1, ctx2) =>
        switch (
          IdTagged.term_of(elab_ctxt(ctx1)),
          IdTagged.term_of(elab_ctxt(ctx2)),
        ) {
        | (Ctx(ps1), Ctx(ps2)) => Ctx(List.fold_left(cons_ctx, ps2, ps1))
        | _ => hole
        }
      // Prop
      | Type(_)
      | HasType(_)
      | Syn(_)
      | Ana(_)
      | And(_)
      | Or(_)
      | Impl(_)
      | Truth
      | Falsity
      // Exp
      | NumLit(_)
      | Neg(_)
      | Plus(_)
      | Minus(_)
      | Times(_)
      | Gt(_)
      | Lt(_)
      | Eq(_)
      | True
      | False
      | If(_)
      | Let(_)
      | Fix(_)
      | Fun(_)
      | Ap(_)
      | Triv
      | PrjL(_)
      | PrjR(_)
      | InjL(_)
      | InjR(_)
      | Case(_)
      | Roll(_)
      | Unroll(_)
      | ExpHole => hole
      };
    {...ctx, term};
  }
and elab_prop: Drv.Exp.t => t =
  prop => {
    let hole: term = Hole(Drv.Exp.show(prop));
    let term: term =
      switch (exp_term_of(prop)) {
      | Hole(s) => Hole(DrvTermBase.show_type_hole(s))
      | Abbr(_)
      | Parens(_)
      | Tuple(_)
      // Jdmt
      | Val(_)
      | Eval(_)
      | Entail(_)
      | Consistent(_)
      | MatchedArrow(_)
      | MatchedProd(_)
      | MatchedSum(_)
      // Ctx
      | Ctx(_)
      | Cons(_)
      | Concat(_) => hole
      // Prop
      | Type(t) => Type(elab_typ(t))
      | HasType(e, t) => HasType(elab_exp(e), elab_typ(t))
      | Syn(e, t) => Syn(elab_exp(e), elab_typ(t))
      | Ana(e, t) => Ana(elab_exp(e), elab_typ(t))
      | Var(x) => Atom(x)
      | And(p1, p2) => And(elab_prop(p1), elab_prop(p2))
      | Or(p1, p2) => Or(elab_prop(p1), elab_prop(p2))
      | Impl(p1, p2) => Impl(elab_prop(p1), elab_prop(p2))
      | Truth => Truth
      | Falsity => Falsity
      // Exp
      | NumLit(_)
      | Neg(_)
      | Plus(_)
      | Minus(_)
      | Times(_)
      | Gt(_)
      | Lt(_)
      | Eq(_)
      | True
      | False
      | If(_)
      | Let(_)
      | Fix(_)
      | Fun(_)
      | Ap(_)
      | Triv
      | PrjL(_)
      | PrjR(_)
      | InjL(_)
      | InjR(_)
      | Case(_)
      | Roll(_)
      | Unroll(_)
      | ExpHole => hole
      };
    {...prop, term};
  }
and elab_exp: Drv.Exp.t => t =
  exp => {
    let rec pat_term_of: Drv.Pat.t => Drv.Pat.term =
      pat =>
        switch (pat.term) {
        | Parens(p) => pat_term_of(p)
        | p => p
        };
    let rec exp_term_of: Drv.Exp.t => Drv.Exp.term =
      exp =>
        switch (exp.term) {
        | Parens(e) => exp_term_of(e)
        | e => e
        };
    let hole: term = Hole(Drv.Exp.show(exp));
    let term: term =
      switch (exp_term_of(exp)) {
      | Hole(s) => Hole(DrvTermBase.show_type_hole(s))
      | Abbr(_)
      | Parens(_)
      // Jdmt
      | Val(_)
      | Eval(_)
      | Entail(_)
      | Consistent(_)
      | MatchedArrow(_)
      | MatchedProd(_)
      | MatchedSum(_)
      // Ctx
      | Ctx(_)
      | Cons(_)
      | Concat(_)
      // Prop
      | Type(_)
      | HasType(_)
      | Syn(_)
      | Ana(_)
      | And(_)
      | Or(_)
      | Impl(_)
      | Truth
      | Falsity => hole
      | NumLit(i) => NumLit(i)
      | Neg(e) => Neg(elab_exp(e))
      | Plus(e1, e2) => Plus(elab_exp(e1), elab_exp(e2))
      | Minus(e1, e2) => Minus(elab_exp(e1), elab_exp(e2))
      | Times(e1, e2) => Times(elab_exp(e1), elab_exp(e2))
      | Lt(e1, e2) => Lt(elab_exp(e1), elab_exp(e2))
      | Gt(e1, e2) => Gt(elab_exp(e1), elab_exp(e2))
      | Eq(e1, e2) => Eq(elab_exp(e1), elab_exp(e2))
      | True => True
      | False => False
      | If(e1, e2, e3) => If(elab_exp(e1), elab_exp(e2), elab_exp(e3))
      | Var(x) => Var(x)
      | Let(p, e1, e2) => Let(elab_pat(p), elab_exp(e1), elab_exp(e2))
      | Fix(p, e) => Fix(elab_pat(p), elab_exp(e))
      | Fun(x, e) => Fun(elab_pat(x), elab_exp(e))
      | Ap(e1, e2) => Ap(elab_exp(e1), elab_exp(e2))
      | Tuple([e1, e2]) => Pair(elab_exp(e1), elab_exp(e2))
      | Tuple(_) => hole
      | Triv => Triv
      | PrjL(e) => PrjL(elab_exp(e))
      | PrjR(e) => PrjR(elab_exp(e))
      | InjL(e) => InjL(elab_exp(e))
      | InjR(e) => InjR(elab_exp(e))
      | Case(e, [(x, e1), (y, e2)]) =>
        let e = elab_exp(e);
        let e1 = elab_exp(e1);
        let e2 = elab_exp(e2);
        switch (pat_term_of(x), pat_term_of(y)) {
        | (InjL(x), InjR(y)) => Case(e, elab_pat(x), e1, elab_pat(y), e2)
        | _ => hole
        };
      | Case(_) => hole
      | Roll(e) => Roll(elab_exp(e))
      | Unroll(e) => Unroll(elab_exp(e))
      | ExpHole => ExpHole
      };
    {...exp, term};
  }
and elab_pat: Drv.Pat.t => t =
  pat => {
    let term: term =
      switch (pat.term) {
      | Hole(s) => Hole(DrvTermBase.show_type_hole(s))
      | Var(x) => Pat(x)
      | Cast(x, t) => Cast(elab_pat(x), elab_typ(t))
      | Pair(x, y) => PatPair(elab_pat(x), elab_pat(y))
      | InjL(x) => InjL(elab_pat(x))
      | InjR(x) => InjR(elab_pat(x))
      | Parens(p) => IdTagged.term_of(elab_pat(p))
      };
    {...pat, term};
  }
and elab_typ: Drv.Typ.t => t =
  typ => {
    let term: term =
      switch (typ.term) {
      | Hole(s) => Hole(DrvTermBase.show_type_hole(s))
      | Abbr(_) => Hole(Drv.Typ.show(typ))
      | Num => Num
      | Bool => Bool
      | Arrow(t1, t2) => Arrow(elab_typ(t1), elab_typ(t2))
      | Prod(t1, t2) => Prod(elab_typ(t1), elab_typ(t2))
      | Unit => Unit
      | Sum(t1, t2) => Sum(elab_typ(t1), elab_typ(t2))
      | Var(x) => TVar(x)
      | Rec(x, t) => Rec(elab_tpat(x), elab_typ(t))
      | TypHole => TypHole
      | Parens(t) => IdTagged.term_of(elab_typ(t))
      };
    {...typ, term};
  }
and elab_tpat: Drv.TPat.t => t =
  tpat => {
    let term: term =
      switch (tpat.term) {
      | Hole(s) => Hole(DrvTermBase.show_type_hole(s))
      | Var(x) => TPat(x)
      };
    {...tpat, term};
  };

let elab_any = (d: Drv.Any.t, s: DrvSort.t) =>
  switch (d, s) {
  | (Exp(e), Jdmt) => elab_jdmt(e)
  | (Exp(e), Ctx) => elab_ctxt(e)
  | (Exp(e), Prop) => elab_prop(e)
  | (Exp(e), Exp) => elab_exp(e)
  | (Pat(p), Pat) => elab_pat(p)
  | (Typ(t), Typ) => elab_typ(t)
  | (TPat(t), TPat) => elab_tpat(t)
  | _ => DrvSyntax.Hole(Drv.Any.show(d)) |> DrvSyntax.fresh
  };
