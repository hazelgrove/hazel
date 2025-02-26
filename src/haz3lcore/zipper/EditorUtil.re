let rec append_exp = (e1: Exp.t, e2: Exp.t): Exp.t => {
  switch (e1.term) {
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | FailedCast(_)
  | Undefined
  | Deferral(_)
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | ListLit(_)
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
  | Cons(_)
  | ListConcat(_)
  | UnOp(_)
  | BinOp(_)
  | BuiltinFun(_)
  | Cast(_)
  | Match(_) => {
      term: Seq(e1, e2),
      annotation: {
        ids: [Id.mk()],
        copied: false,
      },
    }
  | Seq(e11, e12) =>
    let e12' = append_exp(e12, e2);
    {
      term: Seq(e11, e12'),
      annotation: {
        ids: IdTagged.ids(e1),
        copied: false,
      },
    };
  | Filter(kind, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: Filter(kind, ebody'),
      annotation: {
        ids: IdTagged.ids(e1),
        copied: false,
      },
    };
  | Let(p, edef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: Let(p, edef, ebody'),
      annotation: {
        ids: IdTagged.ids(e1),
        copied: false,
      },
    };
  | TyAlias(tp, tdef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: TyAlias(tp, tdef, ebody'),
      annotation: {
        ids: IdTagged.ids(e1),
        copied: false,
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
          term: Constructor("$e", Some(Unknown(Internal) |> Typ.fresh)),
          annotation: {
            copied: false,
            ids: [Id.mk()],
          },
        },
      }),
      term,
    ),
  annotation: {
    copied: false,
    ids: [Id.mk()],
  },
};
