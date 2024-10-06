open DrvSyntax;

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
    let term: term =
      switch (exp_term_of(jdmt)) {
      | Hole(s) => Hole(TermBase.TypeHole.show(s))
      | Val(e) => Val(elab_exp(e))
      | Eval(e1, e2) => Eval(elab_exp(e1), elab_exp(e2))
      | Entail(ctx, p) => Entail(elab_ctxt(ctx), elab_prop(p))
      | _ => Hole(Drv.Exp.show(jdmt))
      };
    {...jdmt, term};
  }
and elab_ctxt: Drv.Exp.t => t =
  ctx => {
    let hole: term = Hole(Drv.Exp.show(ctx));
    let term: term =
      switch (exp_term_of(ctx)) {
      | Hole(s) => Hole(TermBase.TypeHole.show(s))
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
      | _ => hole
      };
    print_endline("elab_ctxt" ++ DrvSyntax.show_term(term));
    {...ctx, term};
  }
and elab_prop: Drv.Exp.t => t =
  prop => {
    let term: term =
      switch (exp_term_of(prop)) {
      | Hole(s) => Hole(TermBase.TypeHole.show(s))
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
      | _ => Hole(Drv.Exp.show(prop))
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
      | Hole(s) => Hole(TermBase.TypeHole.show(s))
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
      | Let(x, e1, e2) =>
        let e1 = elab_exp(e1);
        let e2 = elab_exp(e2);
        switch (pat_term_of(x)) {
        | Var(_) => Let(elab_pat(x), e1, e2)
        | Cast(x, t) => LetAnn(elab_pat(x), elab_typ(t), e1, e2)
        | Pair(x, y) => LetPair(elab_pat(x), elab_pat(y), e1, e2)
        | _ => hole
        };
      | Fix(x, e) =>
        let e = elab_exp(e);
        switch (pat_term_of(x)) {
        | Var(_) => Fix(elab_pat(x), e)
        | Cast(x, t) => FixAnn(elab_pat(x), elab_typ(t), e)
        | _ => hole
        };
      | Fun(x, e) =>
        let e = elab_exp(e);
        switch (pat_term_of(x)) {
        | Var(_) => Fun(elab_pat(x), e)
        | Cast(x, t) => FunAnn(elab_pat(x), elab_typ(t), e)
        | _ => hole
        };
      | Ap(e1, e2) =>
        let e2 = elab_exp(e2);
        switch (exp_term_of(e1)) {
        | InjL => InjL(e2)
        | InjR => InjR(e2)
        | Roll => Roll(e2)
        | Unroll => Unroll(e2)
        | _ => Ap(elab_exp(e1), e2)
        };
      | Tuple([e1, e2]) => Pair(elab_exp(e1), elab_exp(e2))
      | Triv => Triv
      | PrjL(e) => PrjL(elab_exp(e))
      | PrjR(e) => PrjR(elab_exp(e))
      | InjL => hole
      | InjR => hole
      | Case(e, [(x, e1), (y, e2)]) =>
        let e = elab_exp(e);
        let e1 = elab_exp(e1);
        let e2 = elab_exp(e2);
        switch (pat_term_of(x), pat_term_of(y)) {
        | (Ap(l, x), Ap(r, y)) =>
          switch (pat_term_of(l), pat_term_of(r)) {
          | (InjL, InjR) => Case(e, elab_pat(x), e1, elab_pat(y), e2)
          | _ => hole
          }
        | _ => hole
        };
      | Roll => hole
      | Unroll => hole
      | _ => hole
      };
    {...exp, term};
  }
and elab_pat: Drv.Pat.t => t =
  pat => {
    let term: term =
      switch (pat.term) {
      | Hole(s) => Hole(TermBase.TypeHole.show(s))
      | Var(x) => Pat(x)
      | Cast(_)
      | InjL
      | InjR
      | Ap(_)
      | Pair(_) => Hole(Drv.Pat.show(pat))
      | Parens(p) => IdTagged.term_of(elab_pat(p))
      };
    {...pat, term};
  }
and elab_typ: Drv.Typ.t => t =
  typ => {
    let term: term =
      switch (typ.term) {
      | Hole(s) => Hole(TermBase.TypeHole.show(s))
      | Num => Num
      | Bool => Bool
      | Arrow(t1, t2) => Arrow(elab_typ(t1), elab_typ(t2))
      | Prod(t1, t2) => Prod(elab_typ(t1), elab_typ(t2))
      | Unit => Unit
      | Sum(t1, t2) => Sum(elab_typ(t1), elab_typ(t2))
      | Var(x) => TVar(x)
      | Rec(x, t) => Rec(elab_tpat(x), elab_typ(t))
      | Parens(t) => IdTagged.term_of(elab_typ(t))
      };
    {...typ, term};
  }
and elab_tpat: Drv.TPat.t => t =
  tpat => {
    let term: term =
      switch (tpat.term) {
      | Hole(s) => Hole(TermBase.TypeHole.show(s))
      | Var(x) => TPat(x)
      };
    {...tpat, term};
  };
