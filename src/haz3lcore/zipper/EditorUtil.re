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

let rec append_exp = (e1: TermBase.UExp.t, e2: TermBase.UExp.t) => {
  switch (e1.term) {
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | StaticErrorHole(_)
  | DynamicErrorHole(_)
  | FailedCast(_)
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | ListLit(_)
  | Constructor(_)
  | Fun(_)
  | FixF(_)
  | Tuple(_)
  | Var(_)
  | Ap(_)
  | If(_)
  | Test(_)
  | Parens(_)
  | Cons(_)
  | ListConcat(_)
  | UnOp(_)
  | BinOp(_)
  | BuiltinFun(_)
  | Match(_) =>
    TermBase.UExp.{ids: [Id.mk()], copied: false, term: Seq(e1, e2)}
  | Seq(e11, e12) =>
    let e12' = append_exp(e12, e2);
    TermBase.UExp.{ids: e1.ids, copied: false, term: Seq(e11, e12')};
  | Filter(act, econd, ebody) =>
    let ebody' = append_exp(ebody, e2);
    TermBase.UExp.{
      ids: e1.ids,
      copied: false,
      term: Filter(act, econd, ebody'),
    };
  | Let(p, edef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    TermBase.UExp.{ids: e1.ids, copied: false, term: Let(p, edef, ebody')};
  | TyAlias(tp, tdef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    TermBase.UExp.{
      ids: e1.ids,
      copied: false,
      term: TyAlias(tp, tdef, ebody'),
    };
  };
};
