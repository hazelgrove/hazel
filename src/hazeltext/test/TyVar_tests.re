let verbose = false;

let mk_TyAliasLine =
    (ctx, name: string, ty: HTyp.t): (UHExp.line, MetaVarGen.t) => {
  let line =
    UHExp.TyAliasLine(TPat.TyVar(NotInHole, name), UHTyp.contract(ty));
  let (block, _, u_gen) = Statics_Exp.fix_and_renumber_holes(ctx, [line]);
  (List.hd(block), u_gen);
};

let place_cursor_before_first_hole = (e: UHExp.t): option(ZExp.t) => {
  open OptUtil.Syntax;
  let* steps = CursorPath_Exp.next_hole_steps_z(ZExp.place_before(e));
  let* path = CursorPath_Exp.of_steps(steps, e);
  CursorPath_Exp.follow(path, e);
};

let read_with_tyvar =
    (name: string, ty: HTyp.t, text: string)
    : option((Contexts.t, UHExp.t, MetaVarGen.t)) => {
  open OptUtil.Syntax;
  let ctx = Contexts.initial;
  let ctx = Contexts.add_tyvar(ctx, name, Kind.singleton(ty));
  // let+ e = UHExpTest.read(ctx, text);
  let+ e = UHExpTest.read(text);
  let (line, u_gen) = mk_TyAliasLine(ctx, name, HTyp.int);
  (ctx, [line, ...e], u_gen);
};

let construct_word = (word: string): list(Action.t) =>
  String.to_seq(word)
  |> List.of_seq
  |> List.map(String.make(1))
  |> List.map(s => Action.Construct(SChar(s)));

let test_tyvar_actions =
    (name: string, ty: HTyp.t, actions: list(Action.t)): bool => {
  OptUtil.Syntax.(
    {
      let text = Format.sprintf("let x : %s = ? in x", name);
      let* (ctx, e, u_gen) = read_with_tyvar(name, ty, text);
      let* ze = place_cursor_before_first_hole(e);
      switch (ZExpTest.eval(actions, (ze, HTyp.hole, u_gen))) {
      | Ok((ze, ty, _)) =>
        let+ i = Contexts.tyvar_index(ctx, name);
        let t = HTyp.tyvar(i, name);
        if (verbose) {
          Format.printf(
            "%s\n",
            Sexplib.Sexp.to_string_hum(ZExp.sexp_of_t(ze)),
          );
          Format.printf(
            "%s\n",
            Sexplib.Sexp.to_string_hum(HTyp.sexp_of_t(ty)),
          );
        };
        HTyp.equivalent(ctx, ty, t);
      | Error(_) => None
      };
    }
    |> Option.value(~default=false)
  );
};

let test_tyvar_action = (name: string, ty: HTyp.t, a: Action.t): bool =>
  test_tyvar_actions(name, ty, [a]);

// let%test _ = test_tyvar_actions("u", HTyp.int, construct_word("true"));
