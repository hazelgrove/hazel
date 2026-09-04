let rec append_exp = (e1: Language.Exp.t, e2: Language.Exp.t): Language.Exp.t => {
  let mk = (term, e1): Language.Exp.t => {
    term,
    annotation:
      Language.IdTagged.IdTag.mk(
        Language.IdTagged.ids(e1),
        Language.IdTagged.IdTag.empty_secondary,
      ),
  };
  let mk_fresh = (term): Language.Exp.t => {
    term,
    annotation:
      Language.IdTagged.IdTag.mk(
        [Haz3lcore.Id.mk()],
        Language.IdTagged.IdTag.empty_secondary,
      ),
  };
  switch (e1.term) {
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | DynamicErrorHole(_)
  | Undefined
  | Deferral(_)
  | FumolaPeek(_)
  | Atom(_)
  | DrvQuote(_)
  | ListLit(_)
  | TupleExtension(_)
  | ExplicitNonlabel
  | Constructor(_)
  | Closure(_)
  | Fun(_)
  | TypFun(_)
  | FixF(_)
  | Forall(_)
  | Tuple(_)
  | TupLabel(_)
  | Label(_)
  | Dot(_)
  | Var(_)
  | Ap(_)
  | TypAp(_)
  | DeferredAp(_)
  | If(_)
  | Test(_)
  | HintedTest(_)
  | Parens(_)
  | Projector(_)
  | Cons(_)
  | ListConcat(_)
  | LivelitName(_)
  | UnOp(_)
  | BinOp(_)
  | BuiltinFun(_)
  | Asc(_)
  | ProofObject(_)
  | Module(_)
  | ModuleExp(_)
  | Match(_) => mk_fresh(Seq(e1, e2))
  | Seq(e11, e12) =>
    let e12' = append_exp(e12, e2);
    mk(Seq(e11, e12'), e1);
  | Filter(kind, ebody) =>
    let ebody' = append_exp(ebody, e2);
    mk(Filter(kind, ebody'), e1);
  | Let(p, edef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    mk(Let(p, edef, ebody'), e1);
  | Theorem(p, thm, ebody) =>
    let ebody' = append_exp(ebody, e2);
    mk(Theorem(p, thm, ebody'), e1);
  | TyAlias(tp, tdef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    mk(TyAlias(tp, tdef, ebody'), e1);
  | Use(t, ebody) =>
    let ebody' = append_exp(ebody, e2);
    mk(Use(t, ebody'), e1);
  };
};
