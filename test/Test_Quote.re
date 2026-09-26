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

/* ==================== A use of a Macro livelit ==================== */

let pair_def = "{
type Model = (a=SpliceRef, b=SpliceRef);
type Action = Int;
type Expansion = (Int, Int);
let init =
  do a <- new_splice((IntT, Some(IntLit(1)))) in
  do b <- new_splice((IntT, Some(IntLit(2)))) in
  Pure((a=a, b=b));
let update = fun m -> fun a -> Pure(m);
let view = fun m -> Pure(Html.text(\"\"));
let expand = Macro(fun m -> (quote fun x -> fun y -> (y, x) end, [m.a, m.b]))
}";

/* The body names a builtin, to see that a client binding shadowing it does
   not reach in. */
let len_def = "{
type Model = (s=SpliceRef);
type Action = Int;
type Expansion = Int;
let init =
  do s <- new_splice((StringT, Some(IntLit(0)))) in
  Pure((s=s));
let update = fun m -> fun a -> Pure(m);
let view = fun m -> Pure(Html.text(\"\"));
let expand = Macro(fun m -> (quote fun s -> string_length(s) end, [m.s]))
}";

let pair_use = (a, b) =>
  "^pair((a=SpliceRef((\"s1\", "
  ++ a
  ++ ")), b=SpliceRef((\"s2\", "
  ++ b
  ++ "))))";

let bad_expansion = (m: Statics.Map.t): bool =>
  Test_UserLivelits.has_mark(
    fun
    | BadLivelitExpansion(_) => true
    | _ => false,
    m,
  );

/* The Color slide, loaded as the editor loads it, statics and all. */
let slide = (file: string) => {
  let path =
    List.find_opt(
      Sys.file_exists,
      [
        "hazel-programs/docs/livelits/" ++ file,
        "../../../hazel-programs/docs/livelits/" ++ file,
      ],
    )
    |> Option.value(~default="hazel-programs/docs/livelits/" ++ file);
  let ic = open_in_bin(path);
  let text = really_input_string(ic, in_channel_length(ic));
  close_in(ic);
  switch (PersistentZipper.parse_text(~source=file, ~root=Exp, text)) {
  | None => fail(file ++ " did not parse")
  | Some(z) =>
    let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Exp);
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  };
};

let macro_tests = (
  "Quote.Macro",
  [
    test_case(
      "Color (Figure 3): a use means its color",
      `Quick,
      () => {
        let (m, elab) = slide("color-fig3.hz");
        check(bool, "no BadLivelitExpansion", false, bad_expansion(m));
        let v = Evaluator.evaluate(~env=Builtins.env_init, elab) |> fst;
        check(
          Test_Evaluator_Prelude.dhexp_typ,
          "the sliders' values",
          Test_UserLivelits.run("(r = 255, g = 140, b = 0, a = 100)"),
          v,
        );
      },
    ),
    test_case(
      "the use is the quoted function applied to the splices", `Quick, () =>
      Test_UserLivelits.run_test(
        "swapped",
        "(20, 10)",
        "let ^pair = " ++ pair_def ++ " in " ++ pair_use("10", "20"),
      )
    ),
    test_case("splice code runs in the client's scope", `Quick, () =>
      Test_UserLivelits.run_test(
        "k is the client's",
        "(5, 6)",
        "let k = 5 in let ^pair = "
        ++ pair_def
        ++ " in "
        ++ pair_use("k + 1", "k"),
      )
    ),
    test_case("a client's x is not captured by the body's x", `Quick, () =>
      Test_UserLivelits.run_test(
        "x stays the client's",
        "(1, 100)",
        "let x = 100 in let ^pair = "
        ++ pair_def
        ++ " in "
        ++ pair_use("x", "1"),
      )
    ),
    test_case(
      "a client shadowing a builtin does not reach the body", `Quick, () =>
      Test_UserLivelits.run_test(
        "the builtin string_length",
        "3",
        "let ^len = "
        ++ len_def
        ++ " in let string_length = fun s -> 0 in ^len((s=SpliceRef((\"s\", \"abc\"))))",
      )
    ),
    test_case(
      "a splice's code occurs once in the use's elaboration",
      `Quick,
      () => {
        /* Twice would mean it runs twice: once in the model, and again as
           the quoted function's argument. */
        let (_, elab) =
          statics(
            "let ^pair = " ++ pair_def ++ " in " ++ pair_use("424242", "20"),
          );
        let n = ref(0);
        let _ =
          Exp.map_term(
            ~f_exp=
              (continue, e) => {
                switch (e.term) {
                | Atom(Int(i)) when Bigint.to_string(i) == "424242" =>
                  incr(n)
                | _ => ()
                };
                continue(e);
              },
            elab,
          );
        check(int, "occurrences", 1, n^);
      },
    ),
    test_case("a well-typed use has no BadLivelitExpansion", `Quick, () =>
      check(
        bool,
        "none",
        false,
        bad_expansion(
          fst(
            statics(
              "let ^pair = " ++ pair_def ++ " in " ++ pair_use("10", "20"),
            ),
          ),
        ),
      )
    ),
    test_case(
      "a splice of the wrong type is BadLivelitExpansion at the use",
      `Quick,
      () =>
      check(
        bool,
        "marked",
        true,
        bad_expansion(
          fst(
            statics(
              "let ^pair = " ++ pair_def ++ " in " ++ pair_use("\"no\"", "20"),
            ),
          ),
        ),
      )
    ),
  ],
);

/* ==================== Antiquotation ==================== */

let unquote_tests = (
  "Quote.Unquote",
  [
    test_case("an antiquote splices code, it does not compute", `Quick, () =>
      switch (run("quote 1 + unquote IntLit(2) end end").term) {
      | Quote({term: BinOp(_, _, {term: Atom(Int(two)), _}), _})
          when Bigint.to_string(two) == "2" =>
        ()
      | _ => fail("expected the code 1 + 2")
      }
    ),
    test_case(
      "its expression runs in the quotation's scope",
      `Quick,
      () => {
        let (m, _) =
          statics("let k = 5 in quote 1 + unquote IntLit(k) end end");
        check(list(string), "k is not free", [], free_marks(m));
        switch (run("let k = 5 in quote 1 + unquote IntLit(k) end end").term) {
        | Quote({term: BinOp(_, _, {term: Atom(Int(five)), _}), _})
            when Bigint.to_string(five) == "5" =>
          ()
        | _ => fail("expected the code 1 + 5")
        };
      },
    ),
    test_case("outside a quotation it is an error", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        Test_UserLivelits.has_mark(
          fun
          | BadOperator(_) => true
          | _ => false,
          fst(statics("unquote IntLit(1) end")),
        ),
      )
    ),
    test_case(
      "Dynamic Row or Column: a use means its cells' values",
      `Quick,
      () => {
        let (m, elab) = slide("splice-row.hz");
        check(bool, "no BadLivelitExpansion", false, bad_expansion(m));
        let v = Evaluator.evaluate(~env=Builtins.env_init, elab) |> fst;
        check(
          Test_Evaluator_Prelude.dhexp_typ,
          "the three cells init made",
          run("[1, 2, 3]"),
          v,
        );
      },
    ),
  ],
);
