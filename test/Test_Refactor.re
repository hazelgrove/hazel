open Alcotest;
open Haz3lcore;

/* Inline-let: the first term-level refactoring built on the
   segment<->term roundtrip. Caret placement (¦) determines the
   indicated let. */

let text_of = (z: Zipper.t): string =>
  Printer.of_segment(~holes="?", ~refractors=[], Zipper.unselect_and_zip(z));

let collect_tile_ids = (z: Zipper.t): list(Id.t) => {
  let rec go = (seg: Segment.t) =>
    List.concat_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) => [t.id, ...List.concat_map(go, t.children)]
        | _ => []
        },
      seg,
    );
  go(Zipper.unselect_and_zip(z));
};

let inline = (~kind: Action.refactor=InlineLet, marked: string): Zipper.t => {
  let z = Test_Editing.parse_zipper(marked);
  Test_Editing.perform(z, [Action.Refactor(kind)]);
};

let info_map_of = (z: Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  CachedStatics.init_from_term(
    ~settings=Test_Editing.default_settings,
    ~is_dynamic_term=true,
    term,
  ).
    info_map;
};

let kinds_at = (marked: string): list(Action.refactor) => {
  let z = Test_Editing.parse_zipper(marked);
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  Refactor.menu_items(~info_map=info_map_of(z), ~term, z)
  |> List.map(((k, _, _)) => k);
};

let offers = (kind, marked) => List.mem(kind, kinds_at(marked));

let gating_tests = [
  test_case("caret before let", `Quick, () =>
    check(bool, "a", true, offers(InlineLet, "¦let x = 1 in x"))
  ),
  test_case("caret inside let kw", `Quick, () =>
    check(bool, "b", true, offers(InlineLet, "l¦et x = 1 in x"))
  ),
  test_case("caret after let kw", `Quick, () =>
    check(bool, "c", true, offers(InlineLet, "let¦ x = 1 in x"))
  ),
  test_case("occurrence of bound var offers inline", `Quick, () =>
    check(bool, "e", true, offers(InlineLet, "let x = 1 in ¦x"))
  ),
  test_case("caret on pattern var offers inline", `Quick, () =>
    check(bool, "e2", true, offers(InlineLet, "let ¦x = 1 in x"))
  ),
  test_case("caret in def is not a let target", `Quick, () =>
    check(bool, "e3", false, offers(InlineLet, "let x = ¦1 in x + x"))
  ),
  test_case("occurrence resolution respects shadowing", `Quick, () =>
    check(
      bool,
      "e4",
      false,
      offers(InlineLet, "let x = 1 in (fun x -> ¦x)(2)"),
    )
  ),
  test_case("caret on pattern offers remove-unused", `Quick, () =>
    check(bool, "e5", true, offers(RemoveUnusedLet, "let ¦x = 1 in 2"))
  ),
  test_case("unused let offered (statics-gated)", `Quick, () =>
    check(bool, "f", true, offers(RemoveUnusedLet, "¦let x = 1 in 2"))
  ),
  test_case("used let not offered for removal", `Quick, () =>
    check(bool, "g", false, offers(RemoveUnusedLet, "¦let x = 1 in x"))
  ),
  test_case(
    "inline an annotated let",
    `Quick,
    () => {
      let got = inline("¦let x : Int = 5 in x + x") |> text_of;
      check(string, "annotation dropped", "5 + 5", got);
    },
  ),
  test_case(
    "remove an unused annotated let",
    `Quick,
    () => {
      let got =
        inline(~kind=RemoveUnusedLet, "¦let x : Int = 1 in 2") |> text_of;
      check(string, "removed", "2", got);
    },
  ),
  test_case(
    "remove an unused sugar fn",
    `Quick,
    () => {
      let got =
        inline(~kind=RemoveUnusedLet, "¦let f(n) = n + 1 in 2") |> text_of;
      check(string, "removed", "2", got);
    },
  ),
  test_case(
    "remove unused let",
    `Quick,
    () => {
      let got =
        inline(~kind=RemoveUnusedLet, "¦let x = 1 in\n2 + 2") |> text_of;
      check(string, "binding deleted", "2 + 2", got);
    },
  ),
];

let refactor_tests = [
  test_case(
    "single use",
    `Quick,
    () => {
      let got = inline("¦let x = 1 + 2 in x * 3") |> text_of;
      check(string, "inlined with parens", "(1 + 2) * 3", got);
    },
  ),
  test_case(
    "multiple uses get distinct ids",
    `Quick,
    () => {
      let z = inline("¦let y = f(1) in y + y");
      check(string, "both uses inlined", "f(1) + f(1)", text_of(z));
      let ids = collect_tile_ids(z);
      check(
        int,
        "no duplicate tile ids",
        List.length(ids),
        List.length(List.sort_uniq(compare, ids)),
      );
    },
  ),
  test_case(
    "shadowed occurrences untouched",
    `Quick,
    () => {
      let got = inline("¦let x = 1 in (fun x -> x)(x)") |> text_of;
      check(string, "inner x kept", "(fun x -> x)(1)", got);
    },
  ),
  test_case(
    "atomic def needs no parens",
    `Quick,
    () => {
      let got = inline("¦let x = 5 in x + x") |> text_of;
      check(string, "bare literal", "5 + 5", got);
    },
  ),
  test_case(
    "compound def bare when reparse agrees",
    `Quick,
    () => {
      let got = inline("¦let x = 1 + 2 in x + 3") |> text_of;
      check(string, "left-assoc: no parens needed", "1 + 2 + 3", got);
    },
  ),
  test_case(
    "compound def parenthesized when reparse differs",
    `Quick,
    () => {
      let got = inline("¦let x = 1 + 2 in 3 + x") |> text_of;
      check(string, "right operand: parens", "3 + (1 + 2)", got);
    },
  ),
  test_case("non-var pattern is not applicable", `Quick, () =>
    check(
      bool,
      "not offered",
      false,
      offers(InlineLet, "¦let (a, b) = p in a"),
    )
  ),
  test_case(
    "inline from an occurrence",
    `Quick,
    () => {
      let got = inline("let x = 5 in x + ¦x") |> text_of;
      check(string, "inlined via occurrence", "5 + 5", got);
    },
  ),
  test_case(
    "later definitions unaffected",
    `Quick,
    () => {
      let got = inline("¦let x = 1 in\nlet y = 2 in\nx + y") |> text_of;
      check(string, "y survives", "let y = 2 in\n1 + y", got);
    },
  ),
];

let case_tests = [
  test_case(
    "if to case",
    `Quick,
    () => {
      let got = inline(~kind=IfToCase, "¦if a then 1 else 2") |> text_of;
      check(string, "converted", "case a | true => 1 | false => 2 end", got);
    },
  ),
  test_case(
    "case to if",
    `Quick,
    () => {
      let got =
        inline(~kind=CaseToIf, "¦case a | true => 1 | false => 2 end")
        |> text_of;
      check(string, "converted", "if a then 1 else 2", got);
    },
  ),
  test_case(
    "case to if, flipped arms",
    `Quick,
    () => {
      let got =
        inline(~kind=CaseToIf, "¦case a | false => 2 | true => 1 end")
        |> text_of;
      check(string, "converted", "if a then 1 else 2", got);
    },
  ),
  test_case("non-bool case not offered", `Quick, () =>
    check(
      bool,
      "not offered",
      false,
      offers(CaseToIf, "¦case a | 1 => 2 | _ => 3 end"),
    )
  ),
];

let case_arm_tests = [
  test_case(
    "add missing bool arm",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=AddCaseArm,
          "let b : Bool = ? in ¦case b | true => 1 end",
        )
        |> text_of;
      check(
        string,
        "false arm added",
        "let b : Bool = ? in case b | true => 1 | false => ? end",
        got,
      );
    },
  ),
  test_case(
    "add missing constructor arm",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=AddCaseArm,
          "type Color = Red + Green in let c : Color = ? in ¦case c | Red => 1 end",
        )
        |> text_of;
      check(
        string,
        "Green arm added",
        "type Color = Red + Green in let c : Color = ? in case c | Red => 1 | Green => ? end",
        got,
      );
    },
  ),
  test_case(
    "int scrutinee gets wildcard arm",
    `Quick,
    () => {
      let got =
        inline(~kind=AddCaseArm, "let n : Int = ? in ¦case n | 1 => 2 end")
        |> text_of;
      check(
        string,
        "wildcard added",
        "let n : Int = ? in case n | 1 => 2 | _ => ? end",
        got,
      );
    },
  ),
  test_case(
    "multiline case keeps layout",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=AddCaseArm,
          "let b : Bool = ? in ¦case b\n| true => 1\nend",
        )
        |> text_of;
      check(
        string,
        "arm on own line",
        "let b : Bool = ? in case b\n| true => 1\n| false => ?\nend",
        got,
      );
    },
  ),
  test_case("exhaustive case not offered", `Quick, () =>
    check(bool, "no arm", false, offers(AddCaseArm, "¦case a | _ => 1 end"))
  ),
  test_case("unknown scrutinee not offered", `Quick, () =>
    check(bool, "no arm", false, offers(AddCaseArm, "¦case a | 1 => 1 end"))
  ),
];

let annotation_tests = [
  test_case(
    "tuple type annotation gets parens (oracle)",
    `Quick,
    () => {
      let got =
        inline(~kind=AddTypeAnnotation, "¦let p = (1, true) in p")
        |> text_of;
      check(
        string,
        "parenthesized prod",
        "let p : (Int,Bool) = (1, true) in p",
        got,
      );
    },
  ),
  test_case(
    "add type annotation",
    `Quick,
    () => {
      let got =
        inline(~kind=AddTypeAnnotation, "¦let x = 1 + 2 in x") |> text_of;
      check(string, "annotated", "let x : Int = 1 + 2 in x", got);
    },
  ),
  test_case("not offered when type has holes", `Quick, () =>
    check(
      bool,
      "unknown ty",
      false,
      offers(AddTypeAnnotation, "¦let f = fun y -> y in f"),
    )
  ),
  test_case("not offered when already annotated", `Quick, () =>
    check(
      bool,
      "has annotation",
      false,
      offers(AddTypeAnnotation, "¦let x : Int = 1 in x"),
    )
  ),
  test_case(
    "annotation from pattern caret",
    `Quick,
    () => {
      let got =
        inline(~kind=AddTypeAnnotation, "let ¦b = 1 < 2 in b") |> text_of;
      check(string, "bool annotated", "let b : Bool = 1 < 2 in b", got);
    },
  ),
];

let more_tests = [
  test_case(
    "multiline if converts to case, end on its own line",
    `Quick,
    () => {
      let got =
        inline(~kind=IfToCase, "¦if c then\n  1\nelse\n  2")
        |> text_of;
      check(
        string,
        "arm layout kept",
        "case c | true =>\n  1\n| false =>\n  2\nend",
        got,
      );
    },
  ),
  test_case(
    "extract to let",
    `Quick,
    () => {
      let got = inline(~kind=ExtractLet, "1 ¦+ 2") |> text_of;
      check(string, "extracted", "let x = 1 + 2 in x", got);
    },
  ),
  test_case(
    "extract picks fresh name",
    `Quick,
    () => {
      let got =
        inline(~kind=ExtractLet, "let x = 1 in x * f¦(x)") |> text_of;
      check(
        string,
        "fresh x1, hoisted to chain",
        "let x = 1 in let x1 = f(x) in x * x1",
        got,
      );
    },
  ),
  test_case(
    "eta reduce",
    `Quick,
    () => {
      let got = inline(~kind=EtaReduce, "¦fun y -> f(y)") |> text_of;
      check(string, "reduced", "f", got);
    },
  ),
  test_case("eta blocked when var used in fn", `Quick, () =>
    check(bool, "not offered", false, offers(EtaReduce, "¦fun y -> y(y)"))
  ),
  test_case(
    "negate and swap",
    `Quick,
    () => {
      let got =
        inline(~kind=NegateIf, "¦if a && b then 1 else 2") |> text_of;
      check(string, "flipped", "if !(a && b) then 2 else 1", got);
    },
  ),
  test_case(
    "negate preserves arm formatting",
    `Quick,
    () => {
      let got =
        inline(~kind=NegateIf, "¦if a then\n  1 + 1\nelse\n  2 * 2")
        |> text_of;
      check(
        string,
        "multiline arms survive",
        "if !a then\n  2 * 2\nelse\n  1 + 1",
        got,
      );
    },
  ),
  test_case(
    "extract at root line stays bare (oracle)",
    `Quick,
    () => {
      let got = inline(~kind=ExtractLet, "f¦(2), 3") |> text_of;
      check(
        string,
        "let covers the tuple line",
        "let x = f(2) in x, 3",
        got,
      );
    },
  ),
  test_case(
    "extract joins a multiline chain",
    `Quick,
    () => {
      let got =
        inline(~kind=ExtractLet, "let a = 1 in\ng(f¦(2))") |> text_of;
      check(
        string,
        "own line, chain order",
        "let a = 1 in\nlet x = f(2) in\ng(x)",
        got,
      );
    },
  ),
  test_case(
    "extract from a def hoists above the line",
    `Quick,
    () => {
      let got =
        inline(~kind=ExtractLet, "let a = g(f¦(2)) in a + 1") |> text_of;
      check(
        string,
        "above the def line",
        "let x = f(2) in let a = g(x) in a + 1",
        got,
      );
    },
  ),
  test_case(
    "extract stays inside a lambda",
    `Quick,
    () => {
      let got = inline(~kind=ExtractLet, "fun n -> g(f¦(2))") |> text_of;
      check(
        string,
        "fun body is the line",
        "fun n -> let x = f(2) in g(x)",
        got,
      );
    },
  ),
  test_case(
    "extract stays inside a case arm",
    `Quick,
    () => {
      let got =
        inline(~kind=ExtractLet, "case a | 1 => g(f¦(2)) | _ => 0 end")
        |> text_of;
      check(
        string,
        "arm body is the line",
        "case a | 1 => let x = f(2) in g(x) | _ => 0 end",
        got,
      );
    },
  ),
  test_case(
    "extract from a scrutinee wraps the case",
    `Quick,
    () => {
      let got =
        inline(~kind=ExtractLet, "let a = 1 in case f¦(2) | _ => 0 end")
        |> text_of;
      check(
        string,
        "case is the line",
        "let a = 1 in let x = f(2) in case x | _ => 0 end",
        got,
      );
    },
  ),
];

let param_tests = [
  test_case(
    "add parameter: def and call site",
    `Quick,
    () => {
      let got =
        inline(~kind=AddParameter, "¦let f = fun x -> x + 1 in f(2)")
        |> text_of;
      check(
        string,
        "param + hole arg",
        "let f = fun (x, x1) -> x + 1 in f(2, ?)",
        got,
      );
    },
  ),
  test_case(
    "add parameter: extends existing tuple",
    `Quick,
    () => {
      let got =
        inline(~kind=AddParameter, "¦let f = fun (a, b) -> a in f(1, 2)")
        |> text_of;
      check(
        string,
        "third param",
        "let f = fun (a, b, x) -> a in f(1, 2, ?)",
        got,
      );
    },
  ),
  test_case(
    "add parameter: recursive calls patched",
    `Quick,
    () => {
      let got =
        inline(~kind=AddParameter, "¦let f = fun x -> f(x) in f(1)")
        |> text_of;
      check(
        string,
        "both sites",
        "let f = fun (x, x1) -> f(x, ?) in f(1, ?)",
        got,
      );
    },
  ),
  test_case("add parameter offered at let", `Quick, () =>
    check(
      bool,
      "offered",
      true,
      offers(AddParameter, "¦let f = fun x -> x + 1 in f(2)"),
    )
  ),
  test_case("add parameter offered at binder", `Quick, () =>
    check(
      bool,
      "offered",
      true,
      offers(AddParameter, "let ¦f = fun x -> x + 1 in f(2)"),
    )
  ),
  test_case(
    "add parameter: fn-def sugar",
    `Quick,
    () => {
      let got =
        inline(~kind=AddParameter, "¦let f(x) = x + 1 in f(2)")
        |> text_of;
      check(
        string,
        "sugar pat + call",
        "let f(x, x1) = x + 1 in f(2, ?)",
        got,
      );
    },
  ),
  test_case(
    "add parameter: sugar with multiple params",
    `Quick,
    () => {
      let got =
        inline(~kind=AddParameter, "¦let f(a, b) = a in f(1, 2)")
        |> text_of;
      check(string, "third", "let f(a, b, x) = a in f(1, 2, ?)", got);
    },
  ),
  test_case(
    "add parameter: annotated binding rewrites arrow",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=AddParameter,
          "¦let f : Int -> Int = fun x -> x in f(1)",
        )
        |> text_of;
      check(
        string,
        "arrow arg extended",
        "let f : (Int, ?) -> Int = fun (x, x1) -> x in f(1, ?)",
        got,
      );
    },
  ),
  test_case(
    "add parameter: annotated multi-arg arrow",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=AddParameter,
          "¦let f : (Int, Bool) -> Int = fun (a, b) -> a in f(1, true)",
        )
        |> text_of;
      check(
        string,
        "prod extended",
        "let f : (Int, Bool, ?) -> Int = fun (a, b, x) -> a in f(1, true, ?)",
        got,
      );
    },
  ),
  test_case("add parameter gated on alias annotation", `Quick, () =>
    check(
      bool,
      "opaque ann",
      false,
      offers(
        AddParameter,
        "type F = Int -> Int in ¦let f : F = fun x -> x in f(1)",
      ),
    )
  ),
  test_case("add parameter gated on bare use", `Quick, () =>
    check(
      bool,
      "passed as value",
      false,
      offers(AddParameter, "¦let f = fun x -> x in g(f)"),
    )
  ),
  test_case(
    "add parameter: shadowed uses untouched",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=AddParameter,
          "¦let f = fun x -> x in let f = fun y -> y in f(1)",
        )
        |> text_of;
      check(
        string,
        "inner f kept",
        "let f = fun (x, x1) -> x in let f = fun y -> y in f(1)",
        got,
      );
    },
  ),
];

let rename_tests = [
  test_case(
    "rename at sugar param scopes over the RHS",
    `Quick,
    () => {
      let got =
        inline(~kind=RenameFree("q", "n"), "let f(¦n) = q + 1 in f(2)")
        |> text_of;
      check(string, "param bound", "let f(n) = n + 1 in f(2)", got);
    },
  ),
  test_case(
    "rename at sugar fn name covers def and body",
    `Quick,
    () => {
      let got =
        inline(~kind=RenameFree("g", "f"), "let ¦f(n) = g(n) in g(2)")
        |> text_of;
      check(string, "both regions", "let f(n) = f(n) in f(2)", got);
    },
  ),
  test_case(
    "no rename at let delimiters or pattern punctuation",
    `Quick,
    () => {
      let none = marked =>
        kinds_at(marked)
        |> List.exists(k =>
             switch (k) {
             | Action.RenameFree(_, _) => true
             | _ => false
             }
           );
      check(
        bool,
        "delims and comma offer nothing",
        false,
        none("¦let f(a, b) = q + 1 in f(1, 2)")
        || none("let f(a¦, b) = q + 1 in f(1, 2)"),
      );
    },
  ),
  test_case(
    "caret on the fn name renames via it",
    `Quick,
    () => {
      let kinds = kinds_at("let ¦f(n) = q + 1 in f(2)");
      check(
        bool,
        "q->f only",
        true,
        List.mem(Action.RenameFree("q", "f"), kinds)
        && !List.mem(Action.RenameFree("q", "n"), kinds),
      );
    },
  ),
  test_case(
    "caret on a param narrows to it",
    `Quick,
    () => {
      let kinds = kinds_at("fun (¦a, b) -> q + 1");
      check(
        bool,
        "q->a only",
        true,
        List.mem(Action.RenameFree("q", "a"), kinds)
        && !List.mem(Action.RenameFree("q", "b"), kinds),
      );
    },
  ),
  test_case(
    "sugar param not offered for body frees",
    `Quick,
    () => {
      let kinds = kinds_at("let ¦f(n) = n in q + f(1)");
      check(
        bool,
        "q->n absent, q->f present",
        true,
        !List.mem(Action.RenameFree("q", "n"), kinds)
        && List.mem(Action.RenameFree("q", "f"), kinds),
      );
    },
  ),
  test_case(
    "rename at a case-arm pattern",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=RenameFree("w", "v"),
          "case m | Some(¦v) => w + 1 | None => 0 end",
        )
        |> text_of;
      check(
        string,
        "arm body bound",
        "case m | Some(v) => v + 1 | None => 0 end",
        got,
      );
    },
  ),
  test_case(
    "arm rename scoped to its own arm",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=RenameFree("w", "v"),
          "case m | Some(¦v) => w | None => w end",
        )
        |> text_of;
      check(
        string,
        "other arm untouched",
        "case m | Some(v) => v | None => w end",
        got,
      );
    },
  ),
  test_case(
    "rename at an annotated let",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=RenameFree("v", "vel"),
          "let ¦vel : Int = 1 in v + v",
        )
        |> text_of;
      check(string, "asc head", "let vel : Int = 1 in vel + vel", got);
    },
  ),
  test_case(
    "rename free binds at a fun param",
    `Quick,
    () => {
      let got =
        inline(~kind=RenameFree("n", "m"), "fun ¦m -> n + n") |> text_of;
      check(string, "param bound", "fun m -> m + m", got);
    },
  ),
  test_case("fun delimiters offer no renames", `Quick, () => {
    let kinds = kinds_at("¦fun (a, b) -> q + a");
    check(
      bool,
      "none at fun kw",
      false,
      kinds
      |> List.exists(k =>
           switch (k) {
           | Action.RenameFree(_, _) => true
           | _ => false
           }
         ),
    );
  }),
  test_case(
    "rename free binds occurrences",
    `Quick,
    () => {
      let got =
        inline(~kind=RenameFree("v", "vel"), "let ¦vel = 1 in v + v") |> text_of;
      check(string, "both bound", "let vel = 1 in vel + vel", got);
    },
  ),
  test_case(
    "rename skips scopes rebinding the new name",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=RenameFree("x", "y"),
          "let ¦y = 1 in x + (fun y -> x)(2)",
        )
        |> text_of;
      check(
        string,
        "inner x untouched",
        "let y = 1 in y + (fun y -> x)(2)",
        got,
      );
    },
  ),
  test_case(
    "rename leaves bound occurrences alone",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=RenameFree("x", "y"),
          "let ¦y = 1 in x + (let x = 2 in x)",
        )
        |> text_of;
      check(
        string,
        "shadowed x kept",
        "let y = 1 in y + (let x = 2 in x)",
        got,
      );
    },
  ),
  test_case(
    "rename reaches a recursive def",
    `Quick,
    () => {
      let got =
        inline(~kind=RenameFree("f", "g"), "let ¦g = fun n -> f(n) in g(2)")
        |> text_of;
      check(string, "rec call bound", "let g = fun n -> g(n) in g(2)", got);
    },
  ),
  test_case("one menu entry per candidate name", `Quick, () => {
    let kinds = kinds_at("let ¦y = 1 in a + b");
    check(
      bool,
      "both offered",
      true,
      List.mem(Action.RenameFree("a", "y"), kinds)
      && List.mem(Action.RenameFree("b", "y"), kinds),
    );
  }),
  test_case("no offer without free vars in scope", `Quick, () => {
    let kinds = kinds_at("let ¦y = 1 in y + 2");
    check(
      bool,
      "none",
      false,
      kinds
      |> List.exists(k =>
           switch (k) {
           | Action.RenameFree(_, _) => true
           | _ => false
           }
         ),
    );
  }),
];

let move_tests = [
  test_case(
    "hoist through a chain",
    `Quick,
    () => {
      let got =
        inline(~kind=HoistLet, "let a = 1 in ¦let x = 2 in x + a")
        |> text_of;
      check(string, "swapped", "let x = 2 in let a = 1 in x + a", got);
    },
  ),
  test_case(
    "hoist keeps multiline layout",
    `Quick,
    () => {
      let got =
        inline(~kind=HoistLet, "let a = 1 in\n¦let x = 2 in\nx + a")
        |> text_of;
      check(
        string,
        "lines swapped",
        "let x = 2 in\nlet a = 1 in\nx + a",
        got,
      );
    },
  ),
  test_case(
    "hoist out of a lambda",
    `Quick,
    () => {
      let got =
        inline(~kind=HoistLet, "fun n -> ¦let x = 2 in x + n")
        |> text_of;
      check(string, "once, not per call", "let x = 2 in fun n -> x + n", got);
    },
  ),
  test_case("hoist gated on param mention", `Quick, () =>
    check(
      bool,
      "not offered",
      false,
      offers(HoistLet, "fun n -> ¦let x = n in x"),
    )
  ),
  test_case("hoist gated on binder dependency", `Quick, () =>
    check(
      bool,
      "not offered",
      false,
      offers(HoistLet, "let a = 1 in ¦let x = a in x"),
    )
  ),
  test_case(
    "hoist out of a def",
    `Quick,
    () => {
      let got =
        inline(~kind=HoistLet, "let a = (¦let x = 2 in x) in a")
        |> text_of;
      check(
        string,
        "above the line",
        "let x = 2 in let a = (x) in a",
        got,
      );
    },
  ),
  test_case(
    "hoist out of a multiline def keeps line structure",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=HoistLet,
          "let d =\n  ¦let x = 1 in\n  f(x)\nin\nd",
        )
        |> text_of;
      check(
        string,
        "own lines",
        "let x = 1 in\nlet d =\n  f(x)\nin\nd",
        got,
      );
    },
  ),
  test_case(
    "hoist allowed when the crossed name is only shadowed",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=HoistLet,
          "let a = 1 in ¦let x = (let a = 2 in a) in x + a",
        )
        |> text_of;
      check(
        string,
        "shadowed a doesn't block",
        "let x = (let a = 2 in a) in let a = 1 in x + a",
        got,
      );
    },
  ),
  test_case(
    "sink through a chain",
    `Quick,
    () => {
      let got =
        inline(~kind=SinkLet, "¦let x = 2 in let a = 1 in x + a")
        |> text_of;
      check(string, "swapped down", "let a = 1 in let x = 2 in x + a", got);
    },
  ),
  test_case(
    "sink into a lambda",
    `Quick,
    () => {
      let got =
        inline(~kind=SinkLet, "¦let x = 2 in fun n -> x + n")
        |> text_of;
      check(string, "per call", "fun n -> let x = 2 in x + n", got);
    },
  ),
  test_case(
    "sink into the sole using arm",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=SinkLet,
          "¦let x = 2 in case m | 1 => x | _ => 0 end",
        )
        |> text_of;
      check(
        string,
        "conditional now",
        "case m | 1 => let x = 2 in x | _ => 0 end",
        got,
      );
    },
  ),
  test_case(
    "sink keeps multiline chain layout",
    `Quick,
    () => {
      let got =
        inline(~kind=SinkLet, "¦let x = 2 in\nlet a = 1 in\nx + a")
        |> text_of;
      check(
        string,
        "lines swapped",
        "let a = 1 in\nlet x = 2 in\nx + a",
        got,
      );
    },
  ),
  test_case(
    "hoist out of a multiline lambda keeps line structure",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=HoistLet,
          "fun n ->\n  ¦let x = 2 in\n  x + n",
        )
        |> text_of;
      check(
        string,
        "own lines",
        "let x = 2 in\nfun n ->\n  x + n",
        got,
      );
    },
  ),
  test_case("sink gated when both arms use it", `Quick, () =>
    check(
      bool,
      "not offered",
      false,
      offers(SinkLet, "¦let x = 2 in case m | 1 => x | _ => x end"),
    )
  ),
];

let tests = [
  (
    "Refactor",
    refactor_tests
    @ gating_tests
    @ case_tests
    @ annotation_tests
    @ case_arm_tests
    @ param_tests
    @ rename_tests
    @ move_tests
    @ more_tests,
  ),
];
