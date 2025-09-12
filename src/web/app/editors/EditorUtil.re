let rec append_exp = (e1: Language.Exp.t, e2: Language.Exp.t): Language.Exp.t => {
  switch (e1.term) {
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | DynamicErrorHole(_)
  | Undefined
  | Deferral(_)
  | Atom(_)
  | DrvExp(_)
  | ListLit(_)
  | TupleExtension(_)
  | Constructor(_)
  | Closure(_)
  | Fun(_)
  | TypFun(_)
  | FixF(_)
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
  | Probe(_)
  | Cons(_)
  | ListConcat(_)
  | LivelitName(_)
  | UnOp(_)
  | BinOp(_)
  | BuiltinFun(_)
  | Asc(_)
  | ProofObject(_)
  | Forall(_)
  | Match(_) => {
      term: Seq(e1, e2),
      annotation: {
        ids: [Language.Id.mk()],
      },
    }
  | Seq(e11, e12) =>
    let e12' = append_exp(e12, e2);
    {
      term: Seq(e11, e12'),
      annotation: {
        ids: Language.IdTagged.ids(e1),
      },
    };
  | Filter(kind, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: Filter(kind, ebody'),
      annotation: {
        ids: Language.IdTagged.ids(e1),
      },
    };
  | Let(p, edef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: Let(p, edef, ebody'),
      annotation: {
        ids: Language.IdTagged.ids(e1),
      },
    };
  | TyAlias(tp, tdef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: TyAlias(tp, tdef, ebody'),
      annotation: {
        ids: Language.IdTagged.ids(e1),
      },
    };
  | Use(t, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: Use(t, ebody'),
      annotation: {
        ids: Language.IdTagged.ids(e1),
      },
    };
  | Theorem(name, prop, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: Theorem(name, prop, ebody'),
      annotation: {
        ids: Language.IdTagged.ids(e1),
      },
    };
  };
};
