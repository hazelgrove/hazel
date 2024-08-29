let rec append_exp = (e1: Exp.t, e2: Exp.t): Exp.t => {
  Exp.(
    switch (e1.term) {
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | DynamicErrorHole(_)
    | FailedCast(_)
    | Undefined
    | Deferral(_)
    | Bool(_)
    | Int(_)
    | Float(_)
    | String(_)
    | Prop(_)
    | ListLit(_)
    | Constructor(_)
    | Closure(_)
    | Fun(_)
    | TypFun(_)
    | FixF(_)
    | Tuple(_)
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
    | Match(_) => Exp.{ids: [Id.mk()], copied: false, term: Seq(e1, e2)}
    | Seq(e11, e12) =>
      let e12' = append_exp(e12, e2);
      {ids: e1.ids, copied: false, term: Seq(e11, e12')};
    | Filter(kind, ebody) =>
      let ebody' = append_exp(ebody, e2);
      {ids: e1.ids, copied: false, term: Filter(kind, ebody')};
    | Let(p, edef, ebody) =>
      let ebody' = append_exp(ebody, e2);
      {ids: e1.ids, copied: false, term: Let(p, edef, ebody')};
    | TyAlias(tp, tdef, ebody) =>
      let ebody' = append_exp(ebody, e2);
      {ids: e1.ids, copied: false, term: TyAlias(tp, tdef, ebody')};
    }
  );
};
