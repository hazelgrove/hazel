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
  | Triv
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | ListLit(_)
  | Constructor(_)
  | Fun(_)
  | Tuple(_)
  | Var(_)
  | Ap(_)
  | Pipeline(_)
  | If(_)
  | Test(_)
  | Parens(_)
  | Cons(_)
  | ListConcat(_)
  | UnOp(_)
  | BinOp(_)
  | Match(_) => TermBase.UExp.{ids: [Id.mk()], term: Seq(e1, e2)}
  | Seq(e11, e12) =>
    let e12' = append_exp(e12, e2);
    TermBase.UExp.{ids: e1.ids, term: Seq(e11, e12')};
  | Filter(act, econd, ebody) =>
    let ebody' = append_exp(ebody, e2);
    TermBase.UExp.{ids: e1.ids, term: Filter(act, econd, ebody')};
  | Let(p, edef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    TermBase.UExp.{ids: e1.ids, term: Let(p, edef, ebody')};
  | TyAlias(tp, tdef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    TermBase.UExp.{ids: e1.ids, term: TyAlias(tp, tdef, ebody')};
  };
};

let stitch = (editors: list(Editor.t)) => {
  print_endline("new stitchin'");
  let exps =
    List.map(
      (ed: Editor.t) =>
        Util.TimeUtil.measure_time(
          "terms",
          true,
          () => {
            let (term, _) =
              Util.TimeUtil.measure_time("Time: MakeTerm.from_zip:", true, () =>
                MakeTerm.from_zip_for_view(ed.state.zipper)
              );
            term;
          },
        ),
      editors,
    );
  switch (exps) {
  | [] => failwith("cannot stitch zero expressions")
  | [e] => e
  | [e1, ...tl] =>
    let e = List.fold_left(append_exp, e1, tl);
    e;
  };
};
