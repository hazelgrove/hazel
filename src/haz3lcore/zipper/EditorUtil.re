let rec append_exp = (e1: Exp.t, e2: Exp.t): Exp.t => {
  switch (e1.term) {
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | DynamicErrorHole(_)
  | FailedCast(_)
  | Undefined
  | Deferral(_)
  | Atom(_)
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
  | Parens(_)
  | Probe(_)
  | Cons(_)
  | ListConcat(_)
  | UnOp(_)
  | BinOp(_)
  | BuiltinFun(_)
  | Cast(_)
  | LivelitName(_)
  | Match(_) => {
      term: Seq(e1, e2),
      annotation: {
        ids: [Id.mk()],
      },
    }
  | Seq(e11, e12) =>
    let e12' = append_exp(e12, e2);
    {
      term: Seq(e11, e12'),
      annotation: {
        ids: IdTagged.ids(e1),
      },
    };
  | Filter(kind, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: Filter(kind, ebody'),
      annotation: {
        ids: IdTagged.ids(e1),
      },
    };
  | Let(p, edef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: Let(p, edef, ebody'),
      annotation: {
        ids: IdTagged.ids(e1),
      },
    };
  | TyAlias(tp, tdef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: TyAlias(tp, tdef, ebody'),
      annotation: {
        ids: IdTagged.ids(e1),
      },
    };
  | Use(t, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: Use(t, ebody'),
      annotation: {
        ids: IdTagged.ids(e1),
      },
    };
  };
};

let wrap_filter = (act: FilterAction.action, term: Exp.t): Exp.t => {
  term:
    Filter(
      Filter({
        act: FilterAction.(act, One),
        pat: {
          term:
            Constructor("$e", Some(Some(Unknown(Internal) |> Typ.fresh))),
          annotation: {
            ids: [Id.mk()],
          },
        },
      }),
      term,
    ),
  annotation: {
    ids: [Id.mk()],
  },
};
