open Alcotest;
open Haz3lcore;
open Language;

/* `quote e end`: e as code, a value of type Exp (PLDI 2021 Sec. 3.2.5,
   Fig. 3 l.56). */

let statics = Test_UserLivelits.statics;
let run = Test_UserLivelits.run;

let free_marks = (m: Statics.Map.t): list(string) =>
  Id.Map.fold(
    (_, info, acc) =>
      switch ((info: Info.t)) {
      | InfoExp({marks, _}) =>
        List.filter_map(
          fun
          | Mark.Free(x) => Some(x)
          | _ => None,
          marks,
        )
        @ acc
      | _ => acc
      },
    m,
    [],
  )
  |> List.sort_uniq(compare);

let error_count = (m: Statics.Map.t): int =>
  Id.Map.fold(
    (_, info, acc) =>
      switch ((info: Info.t)) {
      | InfoExp({marks, _}) => acc + List.length(marks)
      | _ => acc
      },
    m,
    0,
  );

/* The type the whole program synthesizes. */
let program_ty = (text: string): Typ.t => {
  let term = Test_Evaluator_Prelude.parse_exp(text);
  let (m, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  switch (Id.Map.find_opt(Exp.rep_id(term), m)) {
  | Some(InfoExp({ty, _})) => ty
  | _ => fail("no info for the program")
  };
};

let is_exp_typ = (ty: Typ.t): bool =>
  switch (Typ.term_of(ty)) {
  | Var("Exp") => true
  | _ => false
  };

let typed = code => {
  let z = Test_Editing.perform(Zipper.init(), Test_Editing.mk(code ++ "¦"));
  Zipper.zip(z);
};

let tests = (
  "Quote",
  [
    test_case(
      "typed, it is one quote ... end tile",
      `Quick,
      () => {
        let seg = typed("quote 1 + 2 end");
        check(
          list(string),
          "one tile",
          ["quote end"],
          List.filter_map(
            (p: Piece.t) =>
              switch (p) {
              | Tile(t) => Some(String.concat(" ", t.label))
              | _ => None
              },
            seg,
          ),
        );
        check(
          string,
          "prints back",
          "quote 1 + 2 end",
          EditingPrelude.print_seg(seg),
        );
        switch (MakeTerm.go(seg).term.term) {
        | Quote(_) => ()
        | _ => fail("MakeTerm did not make a Quote")
        };
      },
    ),
    test_case("FastParse takes it, no fallback", `Quick, () =>
      switch (
        FastParse.parsed_of_text(~root=Exp, "let q = quote 1 + 2 end in q")
      ) {
      | Ok(_) => ()
      | Error(why) => fail("FastParse bailed: " ++ why)
      }
    ),
    test_case(
      "it synthesizes Exp",
      `Quick,
      () => {
        check(bool, "Exp", true, is_exp_typ(program_ty("quote 1 + 2 end")));
        check(
          int,
          "no errors",
          0,
          error_count(fst(statics("quote 1 + 2 end"))),
        );
      },
    ),
    test_case("its body is closed: a local is free inside", `Quick, () =>
      check(
        list(string),
        "x is free in the quotation",
        ["x"],
        free_marks(fst(statics("let x = 1 in quote x + 1 end"))),
      )
    ),
    test_case("builtins are in scope inside", `Quick, () =>
      check(
        list(string),
        "nothing free",
        [],
        free_marks(fst(statics("quote string_length(\"abc\") end"))),
      )
    ),
    test_case("Color's parameterized expansion checks", `Quick, () =>
      check(
        int,
        "no errors",
        0,
        error_count(
          fst(
            statics(
              "quote fun r -> fun g -> fun b -> fun a -> (r, g, b, a) end",
            ),
          ),
        ),
      )
    ),
    test_case("it is a value: the body is not evaluated", `Quick, () =>
      switch (run("quote 1 + 2 end").term) {
      | Quote({term: BinOp(_), _}) => ()
      | _ => fail("quote 1 + 2 end did not evaluate to itself")
      }
    ),
    test_case("an IntLit pattern does not match it", `Quick, () =>
      Test_UserLivelits.run_test(
        "falls to the wildcard",
        "0",
        "case quote 1 end | IntLit(n) => n | _ => 0 end",
      )
    ),
    test_case(
      "quote is a reserved word",
      `Quick,
      () => {
        /* Typed on its own, `quote` is the keyword -- the first shard of a
           quote ... end form -- not a variable. */
        let seg = typed("quote");
        check(
          bool,
          "a quote ... end tile, not a variable",
          true,
          List.exists(
            (p: Piece.t) =>
              switch (p) {
              | Tile(t) => t.label == ["quote", "end"]
              | _ => false
              },
            seg,
          ),
        );
      },
    ),
  ],
);
