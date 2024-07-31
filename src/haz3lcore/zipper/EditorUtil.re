let editor_of_code = (~read_only=false, code: CodeString.t) => {
  switch (Printer.zipper_of_string(code)) {
  | None => None
  | Some(z) => Some(Editor.init(~read_only, z))
  };
};

let editors_for =
    (~read_only=false, xs: list('a), f: 'a => option(string))
    : (int, list(('a, option(Editor.t)))) => {
  let zs =
    List.fold_left(
      (acc_zs, a) => {
        switch (f(a)) {
        | Some(str) =>
          switch (Printer.zipper_of_string(str)) {
          | None => acc_zs @ [(a, Some(Zipper.init()))]
          | Some(z) => acc_zs @ [(a, Some(z))]
          }
        | None => acc_zs @ [(a, None)]
        }
      },
      [],
      xs,
    );
  (
    0,
    List.map(
      ((a, sz)) =>
        switch (sz) {
        | Some(z) => (a, Some(Editor.init(z, ~read_only)))
        | None => (a, None)
        },
      zs,
    ),
  );
};

let editors_of_strings = (~read_only=false, xs: list(string)) => {
  let (i, aes) = editors_for(xs, x => Some(x), ~read_only);
  (i, List.map(((_, oe)) => Option.get(oe), aes));
};

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

let wrap_filter = (act: FilterAction.action, term: UExp.t): UExp.t =>
  Exp.{
    term:
      Exp.Filter(
        Filter({
          act: FilterAction.(act, One),
          pat: {
            term: Constructor("$e", Unknown(Internal) |> Typ.fresh),
            copied: false,
            ids: [Id.mk()],
          },
        }),
        term,
      ),
    copied: false,
    ids: [Id.mk()],
  };
