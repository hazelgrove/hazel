open Haz3lcore;

let editor_of_code = (~read_only=false, init_id, code: CodeString.t) => {
  switch (Printer.zipper_of_string(init_id, code)) {
  | None => None
  | Some((z, new_id)) => Some((new_id, Editor.init(~read_only, z)))
  };
};

let editors_for =
    (~read_only=false, xs: list('a), f: 'a => option(string))
    : (Id.t, int, list(('a, option(Editor.t)))) => {
  let (id_gen, zs) =
    List.fold_left(
      ((acc_id, acc_zs), a) => {
        switch (f(a)) {
        | Some(str) =>
          switch (Printer.zipper_of_string(acc_id, str)) {
          | None => (acc_id, acc_zs @ [(a, Some(Zipper.init(0)))])
          | Some((z, new_id)) => (new_id, acc_zs @ [(a, Some(z))])
          }
        | None => (acc_id, acc_zs @ [(a, None)])
        }
      },
      (0, []),
      xs,
    );
  (
    id_gen,
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
  let (id, i, aes) = editors_for(xs, x => Some(x), ~read_only);
  (id, i, List.map(((_, oe)) => Option.get(oe), aes));
};

let info_map = (editor: Editor.t) => {
  let zipper = editor.state.zipper;
  let unselected = Zipper.unselect_and_zip(zipper);
  let (term, _) =
    Util.TimeUtil.measure_time("EditorUtil.info_map => MakeTerm.go", true, () =>
      MakeTerm.go(unselected)
    );
  let info_map = Statics.mk_map(term);
  info_map;
};

let rec append_exp = (id, e1: TermBase.UExp.t, e2: TermBase.UExp.t) => {
  switch (e1.term) {
  | Invalid(_)
  | EmptyHole
  | MultiHole(_)
  | Triv
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | ListLit(_)
  | Tag(_)
  | Fun(_)
  | Tuple(_)
  | Var(_)
  | Ap(_)
  | If(_)
  | Seq(_)
  | Test(_)
  | Parens(_)
  | Cons(_)
  | UnOp(_)
  | BinOp(_)
  | Match(_) => (
      id + 1,
      TermBase.UExp.{
        ids: [id + 10_000_000 /* hack to get unique ID */],
        term: Seq(e1, e2),
      },
    )
  | Let(p, edef, ebody) =>
    let (id, ebody') = append_exp(id, ebody, e2);
    (id, TermBase.UExp.{ids: e1.ids, term: Let(p, edef, ebody')});
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
            let seg =
              Util.TimeUtil.measure_time("unselectin", true, () =>
                Zipper.unselect_and_zip(ed.state.zipper)
              );
            let (term, _) =
              Util.TimeUtil.measure_time("makin terms", true, () =>
                MakeTerm.go(seg)
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
    let (_, e) =
      List.fold_left(
        ((id, e1), e2) => append_exp(id, e1, e2),
        (0, e1),
        tl,
      );
    e;
  };
};
