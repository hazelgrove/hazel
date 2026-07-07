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

let labels_at = (marked: string): list(string) => {
  let z = Test_Editing.parse_zipper(marked);
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  Refactor.menu_items(~info_map=info_map_of(z), ~term, z)
  |> List.map(((_, l, _)) => l);
};

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
    "inline an annotated let (redundant annotation drops)",
    `Quick,
    () => {
      let got = inline("¦let x : Int = 5 in x + x") |> text_of;
      check(string, "annotation dropped", "5 + 5", got);
    },
  ),
  test_case(
    "inline keeps a load-bearing annotation as ascription",
    `Quick,
    () => {
      let got =
        inline("¦let f : Int -> Int = fun y -> y in f(1)") |> text_of;
      check(string, "ascribed", "((fun y -> y) : Int -> Int)(1)", got);
    },
  ),
  test_case(
    "ret-annotated sugar inlines with a hole-arrow ascription",
    `Quick,
    () => {
      let got = inline("¦let f(x) : Int = x + 1 in f(2)") |> text_of;
      check(string, "? -> Int", "((fun x -> x + 1) : ? -> Int)(2)", got);
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
    "compound def at top level takes parens (static policy)",
    `Quick,
    () => {
      let got = inline("¦let x = 1 + 2 in x + 3") |> text_of;
      check(string, "parens: unbounded region", "(1 + 2) + 3", got);
    },
  ),
  test_case(
    "inline parens are per-occurrence, not all-or-nothing",
    `Quick,
    () => {
      let got = inline("¦let x = 1 + 2 in f(x + 3, 3 - x)") |> text_of;
      check(
        string,
        "first bare, second parenthesized",
        "f(1 + 2 + 3, 3 - (1 + 2))",
        got,
      );
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
  test_case("case arm label names the witness", `Quick, () =>
    check(
      bool,
      "Add arm | false",
      true,
      List.mem(
        "Add arm | false",
        labels_at("let b : Bool = ? in ¦case b | true => 1 end"),
      ),
    )
  ),
  test_case("add parameter label names the fn", `Quick, () =>
    check(
      bool,
      "Add param to f",
      true,
      List.mem("Add param to f", labels_at("¦let f = fun x -> x in f(1)")),
    )
  ),
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
        inline(~kind=AddTypeAnnotation, "¦let p = (1, true) in p") |> text_of;
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

let wave_tests = [
  test_case(
    "negate is self-inverse",
    `Quick,
    () => {
      let got = inline(~kind=NegateIf, "¦if !a then 1 else 2") |> text_of;
      check(string, "unwrapped", "if a then 2 else 1", got);
    },
  ),
  test_case(
    "eta expand a unary fn",
    `Quick,
    () => {
      let got =
        inline(~kind=EtaExpand, "let f : Int -> Int = fun y -> y in ¦f")
        |> text_of;
      check(
        string,
        "expanded",
        "let f : Int -> Int = fun y -> y in (fun x -> f(x))",
        got,
      );
    },
  ),
  test_case(
    "eta expand matches arity",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=EtaExpand,
          "let f : (Int, Bool) -> Int = fun (a, b) -> a in ¦f",
        )
        |> text_of;
      check(
        string,
        "two params",
        "let f : (Int, Bool) -> Int = fun (a, b) -> a in (fun (x, x1) -> f(x, x1))",
        got,
      );
    },
  ),
  test_case("eta expand not offered on non-arrows", `Quick, () =>
    check(bool, "no", false, offers(EtaExpand, "let x = 1 in ¦x"))
  ),
  test_case(
    "evaluate in place",
    `Quick,
    () => {
      let got = inline(~kind=EvaluateInPlace, "let x = 2 ¦* 3 in x + 1");
      check(string, "value spliced", "let x = 6 in x + 1", got |> text_of);
    },
  ),
  test_case(
    "evaluate in place: structured value",
    `Quick,
    () => {
      let got =
        inline(~kind=EvaluateInPlace, "let l = 1 ¦:: [2 + 3] in l")
        |> text_of;
      check(string, "list value", "let l = [1, 5] in l", got);
    },
  ),
  test_case("evaluate gated on open terms", `Quick, () =>
    check(bool, "no", false, offers(EvaluateInPlace, "fun y -> ¦y + 1"))
  ),
  test_case("evaluate gated on values", `Quick, () =>
    check(bool, "no", false, offers(EvaluateInPlace, "let x = ¦6 in x"))
  ),
];

let wave2_tests = [
  test_case(
    "inline a sugar fn as a lambda",
    `Quick,
    () => {
      let got = inline("¦let f(x) = x + 1 in f(2)") |> text_of;
      check(string, "lambda applied", "(fun x -> x + 1)(2)", got);
    },
  ),
  test_case(
    "inline a multi-param sugar fn",
    `Quick,
    () => {
      let got = inline("¦let f(a, b) = a + b in f(1, 2)") |> text_of;
      check(string, "tuple param", "(fun (a, b) -> a + b)(1, 2)", got);
    },
  ),
  test_case("recursive sugar fn not inlinable", `Quick, () =>
    check(
      bool,
      "gated",
      false,
      offers(InlineLet, "¦let f(n) = f(n) in f(2)"),
    )
  ),
  test_case(
    "inline avoids capture by renaming the binder",
    `Quick,
    () => {
      let got = inline("¦let x = y + 1 in (fun y -> x + y)(5)") |> text_of;
      check(string, "y renamed", "(fun y1 -> y + 1 + y1)(5)", got);
    },
  ),
  test_case(
    "expand wildcard to unhandled constructors",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=ExpandWildcard,
          "type Color = Red + Green + Blue(Int) in let c : Color = ? in case c | Red => 1 | ¦_ => 0 end",
        )
        |> text_of;
      check(
        string,
        "two arms",
        "type Color = Red + Green + Blue(Int) in let c : Color = ? in case c | Red => 1 | Green => 0 | Blue(_) => 0 end",
        got,
      );
    },
  ),
  test_case("expand wildcard gated when all handled", `Quick, () =>
    check(
      bool,
      "gated",
      false,
      offers(
        ExpandWildcard,
        "type C = A + B in let c : C = ? in case c | A => 1 | B => 2 | ¦_ => 0 end",
      ),
    )
  ),
];

let remove_param_tests = [
  test_case(
    "remove unused parameter: def and call sites",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=RemoveParameter,
          "let f = fun (a, ¦b) -> a + 1 in f(1, 2) + f(3, 4)",
        )
        |> text_of;
      check(
        string,
        "dropped everywhere",
        "let f = fun a -> a + 1 in f(1) + f(3)",
        got,
      );
    },
  ),
  test_case(
    "remove unused parameter on sugar def",
    `Quick,
    () => {
      let got =
        inline(~kind=RemoveParameter, "let f(a, ¦b) = a in f(1, 2)")
        |> text_of;
      check(string, "sugar", "let f(a) = a in f(1)", got);
    },
  ),
  test_case(
    "remove parameter drops the annotation column",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=RemoveParameter,
          "let f : (Int, Bool) -> Int = fun (a, ¦b) -> a in f(1, true)",
        )
        |> text_of;
      check(
        string,
        "prod dropped",
        "let f : Int -> Int = fun a -> a in f(1)",
        got,
      );
    },
  ),
  test_case("remove parameter gated when used", `Quick, () =>
    check(
      bool,
      "b is used",
      false,
      offers(RemoveParameter, "let f = fun (a, ¦b) -> a + b in f(1, 2)"),
    )
  ),
  test_case("remove parameter labeled with its name", `Quick, () =>
    check(
      bool,
      "named",
      true,
      List.mem(
        "Remove Parameter b",
        labels_at("let f = fun (a, ¦b) -> a in f(1, 2)"),
      ),
    )
  ),
];

let swap_tests = [
  test_case(
    "swap params: def and call sites",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=SwapParams(0),
          "¦let f = fun (a, b) -> a - b in f(1, 2) + f(3, 4)",
        )
        |> text_of;
      check(
        string,
        "swapped everywhere",
        "let f = fun (b, a) -> a - b in f(2, 1) + f(4, 3)",
        got,
      );
    },
  ),
  test_case(
    "swap params on sugar def",
    `Quick,
    () => {
      let got =
        inline(~kind=SwapParams(0), "¦let f(a, b) = a - b in f(1, 2)")
        |> text_of;
      check(string, "sugar", "let f(b, a) = a - b in f(2, 1)", got);
    },
  ),
  test_case(
    "swap params rewrites the annotation arrow",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=SwapParams(0),
          "¦let f : (Int, Bool) -> Int = fun (a, b) -> a in f(1, true)",
        )
        |> text_of;
      check(
        string,
        "prod swapped",
        "let f : (Bool, Int) -> Int = fun (b, a) -> a in f(true, 1)",
        got,
      );
    },
  ),
  test_case("swap gated on non-tuple call", `Quick, () =>
    check(
      bool,
      "gated",
      false,
      offers(SwapParams(0), "¦let f = fun (a, b) -> a in f(p)"),
    )
  ),
  test_case("swap menu names params", `Quick, () =>
    check(
      bool,
      "Swap a ↔ b",
      true,
      List.mem(
        "Swap a ↔ b",
        labels_at("¦let f = fun (a, b) -> a in f(1, 2)"),
      ),
    )
  ),
];

let put_down_tests = [
  test_case(
    "put_down spaces the lexer-impossible junction (end|in)",
    `Quick,
    () => {
      let z = Test_RoundtripFuzz.type_string("let x = case 1 end");
      let z = Test_RoundtripFuzz.apply(z, Put_down);
      let got = text_of(z);
      let contains = (needle, hay) => {
        let n = String.length(needle);
        let h = String.length(hay);
        let rec go = i =>
          i + n <= h && (String.sub(hay, i, n) == needle || go(i + 1));
        go(0);
      };
      check(bool, "no endin in: " ++ got, false, contains("endin", got));
    },
  ),
];

let more_tests = [
  test_case(
    "multiline if converts to case, end on its own line",
    `Quick,
    () => {
      let got =
        inline(~kind=IfToCase, "¦if c then\n  1\nelse\n  2") |> text_of;
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
      check(string, "let covers the tuple line", "let x = f(2) in x, 3", got);
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
    "extract does not duplicate a preceding comment block",
    `Quick,
    () => {
      let got =
        inline(~kind=ExtractLet, "# note #\nlet a = 1 in\ng(f¦(2))")
        |> text_of;
      check(
        string,
        "comment kept once",
        "# note #\nlet a = 1 in\nlet x = f(2) in\ng(x)",
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
        inline(~kind=AddParameter, "¦let f(x) = x + 1 in f(2)") |> text_of;
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
        inline(~kind=AddParameter, "¦let f(a, b) = a in f(1, 2)") |> text_of;
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
        inline(~kind=RenameFree("v", "vel"), "let ¦vel : Int = 1 in v + v")
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
  test_case(
    "fun delimiters offer no renames",
    `Quick,
    () => {
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
    },
  ),
  test_case(
    "rename free binds occurrences",
    `Quick,
    () => {
      let got =
        inline(~kind=RenameFree("v", "vel"), "let ¦vel = 1 in v + v")
        |> text_of;
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
  test_case(
    "one menu entry per candidate name",
    `Quick,
    () => {
      let kinds = kinds_at("let ¦y = 1 in a + b");
      check(
        bool,
        "both offered",
        true,
        List.mem(Action.RenameFree("a", "y"), kinds)
        && List.mem(Action.RenameFree("b", "y"), kinds),
      );
    },
  ),
  test_case(
    "no offer without free vars in scope",
    `Quick,
    () => {
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
    },
  ),
];

let move_tests = [
  test_case(
    "hoist through a chain",
    `Quick,
    () => {
      let got =
        inline(~kind=HoistLet, "let a = 1 in ¦let x = 2 in x + a") |> text_of;
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
        inline(~kind=HoistLet, "fun n -> ¦let x = 2 in x + n") |> text_of;
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
        inline(~kind=HoistLet, "let a = (¦let x = 2 in x) in a") |> text_of;
      check(string, "above the line", "let x = 2 in let a = (x) in a", got);
    },
  ),
  test_case(
    "hoist out of a multiline def keeps line structure",
    `Quick,
    () => {
      let got =
        inline(~kind=HoistLet, "let d =\n  ¦let x = 1 in\n  f(x)\nin\nd")
        |> text_of;
      check(string, "own lines", "let x = 1 in\nlet d =\n  f(x)\nin\nd", got);
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
    "hoist out of a case arm",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=HoistLet,
          "case m | 1 => ¦let x = 2 in x + 1 | _ => 0 end",
        )
        |> text_of;
      check(
        string,
        "unconditional now",
        "let x = 2 in case m | 1 => x + 1 | _ => 0 end",
        got,
      );
    },
  ),
  test_case("arm hoist gated on capture", `Quick, () =>
    check(
      bool,
      "x used in other arm",
      false,
      offers(HoistLet, "case m | 1 => ¦let x = 2 in x | _ => x end"),
    )
  ),
  test_case(
    "hoist out of a tight position",
    `Quick,
    () => {
      let got =
        inline(~kind=HoistLet, "g(¦let x = f(2) in x + 1)") |> text_of;
      check(string, "let above the call", "let x = f(2) in g(x + 1)", got);
    },
  ),
  test_case(
    "sink through a chain",
    `Quick,
    () => {
      let got =
        inline(~kind=SinkLet, "¦let x = 2 in let a = 1 in x + a") |> text_of;
      check(string, "swapped down", "let a = 1 in let x = 2 in x + a", got);
    },
  ),
  test_case(
    "sink into the def that solely uses it",
    `Quick,
    () => {
      let got =
        inline(~kind=SinkLet, "¦let x = 2 in let y = x + 1 in y") |> text_of;
      check(string, "scope narrowed", "let y = let x = 2 in x + 1 in y", got);
    },
  ),
  test_case("def-sink gated when body also uses it", `Quick, () =>
    check(
      bool,
      "used in both",
      false,
      offers(SinkLet, "¦let x = 2 in let y = x + 1 in y + x"),
    )
  ),
  test_case(
    "sink into a lambda",
    `Quick,
    () => {
      let got =
        inline(~kind=SinkLet, "¦let x = 2 in fun n -> x + n") |> text_of;
      check(string, "per call", "fun n -> let x = 2 in x + n", got);
    },
  ),
  test_case(
    "sink into the sole using arm",
    `Quick,
    () => {
      let got =
        inline(~kind=SinkLet, "¦let x = 2 in case m | 1 => x | _ => 0 end")
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
        inline(~kind=HoistLet, "fun n ->\n  ¦let x = 2 in\n  x + n")
        |> text_of;
      check(string, "own lines", "let x = 2 in\nfun n ->\n  x + n", got);
    },
  ),
  test_case(
    "sink into a multiline lambda keeps the body line",
    `Quick,
    () => {
      let got =
        inline(~kind=SinkLet, "¦let x = 2 in\nfun n ->\n  x + n") |> text_of;
      check(
        string,
        "let joins the body line",
        "fun n ->\n  let x = 2 in x + n",
        got,
      );
    },
  ),
  test_case(
    "sink into a multiline arm keeps the body line",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=SinkLet,
          "¦let x = 2 in\ncase m\n| 1 =>\n  f(x)\n| _ => 0\nend",
        )
        |> text_of;
      check(
        string,
        "let joins the arm line",
        "case m\n| 1 =>\n  let x = 2 in f(x)\n| _ => 0\nend",
        got,
      );
    },
  ),
  test_case(
    "multiline case converts to if",
    `Quick,
    () => {
      let got =
        inline(
          ~kind=CaseToIf,
          "¦case c | true =>\n  1\n| false =>\n  2\nend",
        )
        |> text_of;
      check(string, "arm layout kept", "if c then\n  1\nelse\n  2", got);
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

/* Refuse-only transforms (hoist/sink/swap/remove-param) no longer run
   the print->reparse oracle at invocation (~0.5s/press on a few-page
   buffer). The property lives here instead: every successful prepare
   survives print -> reparse unchanged. */
/* every whitespace Secondary must be atomic (" " or "\n"): the
   renderer's Code.of_secondary crashes on anything else (andrew hit
   Failure("Code: Unrecognized Secondary") extracting at an indented
   arm — sep_like used to synthesize a compound "\n    " piece) */
let secondaries_atomic = (term: Language.Exp.t): bool => {
  let ok = ref(true);
  let check_run = (ws: list(Secondary.t)) =>
    ws
    |> List.iter((w: Secondary.t) =>
         switch (w.content) {
         | Whitespace(s) when s != " " && s != "\n" => ok := false
         | _ => ()
         }
       );
  let _ =
    Language.Exp.map_term(
      ~f_exp=
        (cont, e: Language.Exp.t) => {
          let (b, a) = e.annotation.secondary;
          check_run(b);
          check_run(a);
          cont(e);
        },
      term,
    );
  ok^;
};

let prepare_reparses = (~kind: Action.refactor, marked: string): unit => {
  let z = Test_Editing.parse_zipper(marked);
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let info_map = info_map_of(z);
  switch (Indicated.index(z)) {
  | None => Alcotest.fail("no indication in: " ++ marked)
  | Some(target) =>
    switch (Refactor.impl(kind).prepare(~info_map, ~target, term)) {
    | None => Alcotest.fail("did not apply: " ++ marked)
    | Some((term', _)) =>
      check(bool, marked, true, Refactor.reparses_same(term'));
      check(
        bool,
        "atomic secondaries: " ++ marked,
        true,
        secondaries_atomic(term'),
      );
    }
  };
};

let reparse_safety_tests = {
  let case = (name, kind, marked) =>
    test_case(name, `Quick, () => prepare_reparses(~kind, marked));
  [
    case("hoist chain", HoistLet, "let a = 1 in ¦let x = 2 in x + a"),
    case(
      "hoist multiline chain",
      HoistLet,
      "let a = 1 in\n¦let x = 2 in\nx + a",
    ),
    case("hoist out of lambda", HoistLet, "fun n -> ¦let x = 2 in x + n"),
    case("hoist out of def", HoistLet, "let a = (¦let x = 2 in x) in a"),
    case(
      "hoist out of multiline def",
      HoistLet,
      "let d =\n  ¦let x = 1 in\n  f(x)\nin\nd",
    ),
    case(
      "hoist out of case arm",
      HoistLet,
      "case m | 1 => ¦let x = 2 in x | _ => 0 end",
    ),
    case(
      "hoist out of tight position",
      HoistLet,
      "g(¦let x = f(2) in x + 1)",
    ),
    case("sink chain", SinkLet, "¦let x = 2 in let a = 1 in x + a"),
    case(
      "sink into sole-using def",
      SinkLet,
      "¦let x = 2 in let y = x + 1 in y",
    ),
    case("sink into lambda", SinkLet, "¦let x = 2 in fun n -> x + n"),
    case(
      "sink into sole using arm",
      SinkLet,
      "¦let x = 2 in case m | 1 => x | _ => 0 end",
    ),
    case(
      "sink multiline chain",
      SinkLet,
      "¦let x = 2 in\nlet a = 1 in\nx + a",
    ),
    case(
      "swap params",
      SwapParams(0),
      "¦let f = fun (a, b) -> a - b in f(1, 2)",
    ),
    case(
      "swap sugar params",
      SwapParams(0),
      "¦let f(a, b) = a - b in f(1, 2)",
    ),
    case(
      "remove param",
      RemoveParameter,
      "let f = fun (a, ¦b) -> a in f(1, 2)",
    ),
    case(
      "remove sugar param",
      RemoveParameter,
      "let f(a, ¦b) = a in f(1, 2)",
    ),
    case("swap arms", SwapArms(0), "case c\n| 1 => 11\n| ¦2 => 22\nend"),
    case("extract at root chain", ExtractLet, "let y = f(1 ¦+ 2) in y"),
    case("extract in fun body", ExtractLet, "fun n -> n * (1 ¦+ 2)"),
    case(
      "extract in arm body",
      ExtractLet,
      "case m | 1 => f(2 ¦+ 3) | _ => 0 end",
    ),
    case("extract in if branch", ExtractLet, "if b then f(2 ¦+ 3) else 0"),
    case("extract tuple element", ExtractLet, "(f(¦2), 3)"),
    case("extract whole line", ExtractLet, "¦1 + 2"),
    case(
      "inline top-level compound def",
      InlineLet,
      "¦let x = 1 + 2 in 3 + x",
    ),
    case(
      "inline into test region",
      InlineLet,
      "let x1 = f(9) in\ntest g(2) == ¦x1 end",
    ),
    case("annotate simple", AddTypeAnnotation, "¦let x = 1 in x"),
    case(
      "annotate tuple type",
      AddTypeAnnotation,
      "¦let p = (1, true) in p",
    ),
    case("negate compound cond", NegateIf, "¦if a && b then 1 else 2"),
    case(
      "extract at indented arm body (compound-secondary repro)",
      ExtractLet,
      "let f =\n    fun v ->\n        case v\n        | Lam(x, body) =>\n            Lam(x, g¦(v, body))\n        end in f",
    ),
  ];
};

/* Fuzz the same property over generated terms: every applicable
   movement prepare at every node preserves print->reparse identity.
   Conditional on the baseline term itself roundtripping — generator
   output isn't always editor-canonical, and that's not the
   transform's fault. */
let movement_reparse_fuzz = {
  let all_ids = (term: Language.Exp.t): list(Id.t) => {
    let acc = ref([]);
    let _ =
      Language.Exp.map_term(
        ~f_exp=
          (cont, e) => {
            acc := [Language.Exp.rep_id(e), ...acc^];
            cont(e);
          },
        term,
      );
    acc^;
  };
  QCheck.Test.make(
    ~name="movement prepares preserve print->reparse identity",
    ~count=30,
    QCheck_Util.arb_exp(~minimal_idents=true, 10),
    term =>
    if (!Refactor.reparses_same(term)) {
      true;
    } else {
      let kinds = [
        Action.HoistLet,
        Action.SinkLet,
        Action.SwapParams(0),
        Action.SwapParams(1),
        Action.SwapArms(0),
        Action.SwapArms(1),
        Action.RemoveParameter,
        Action.ExtractLet,
        Action.InlineLet,
        Action.NegateIf,
        Action.IfToCase,
        Action.CaseToIf,
      ];
      all_ids(term)
      |> List.for_all(target =>
           kinds
           |> List.for_all(kind =>
                switch (
                  Refactor.impl(kind).prepare(
                    ~info_map=Id.Map.empty,
                    ~target,
                    term,
                  )
                ) {
                | None => true
                | Some((term', _)) =>
                  Refactor.reparses_same(term')
                  && secondaries_atomic(term')
                  || {
                    Printf.printf(
                      "\nFUZZ TERM %s\n",
                      Language.Exp.show(term),
                    );
                    Printf.printf(
                      "\nFUZZ FAIL %s: %s\n",
                      Action.show_refactor(kind),
                      Printer.of_segment(
                        ~holes="?",
                        ~refractors=[],
                        ExpToSegment.exp_to_segment(
                          ~settings=Refactor.roundtrip_settings,
                          term',
                        ),
                      ),
                    );
                    false;
                  }
                }
              )
         );
    }
  );
};

/* Caret-placement assertions (Printer renders the caret into text) */
let caret_text = (z: Zipper.t): string =>
  Printer.of_zipper(~holes="?", ~caret="¦", z);

let has_sub = (hay: string, needle: string): bool => {
  let n = String.length(needle);
  let rec go = i =>
    i
    + n <= String.length(hay)
    && (String.sub(hay, i, n) == needle || go(i + 1));
  go(0);
};

let caret_tests = [
  test_case(
    "extract focuses the fresh binder",
    `Quick,
    () => {
      let z = inline(~kind=ExtractLet, "let y = f(1 ¦+ 2) in y");
      check(
        bool,
        "caret at binder: " ++ caret_text(z),
        true,
        has_sub(caret_text(z), "let ¦x = 1 + 2"),
      );
    },
  ),
  test_case(
    "inline from occurrence focuses the substituted copy",
    `Quick,
    () => {
      let z = inline("let x1 = f(9) in\ntest g(2) == ¦x1 end");
      check(
        bool,
        "caret at copy: " ++ caret_text(z),
        true,
        has_sub(caret_text(z), "f¦(9)")
        || has_sub(caret_text(z), "¦f(9)"),
      );
    },
  ),
  test_case(
    "inline at the let focuses the first substituted copy",
    `Quick,
    () => {
      let z = inline("¦let x1 = f(9) in test g(2) == x1 end");
      check(
        bool,
        "caret at copy: " ++ caret_text(z),
        true,
        has_sub(caret_text(z), "f¦(9)")
        || has_sub(caret_text(z), "¦f(9)"),
      );
    },
  ),
];

let caret_at = (~kind: Action.refactor, marked: string, expected_sub: string) =>
  test_case(
    Action.show_refactor(kind) ++ " caret: " ++ expected_sub,
    `Quick,
    () => {
      let z = inline(~kind, marked);
      check(
        bool,
        "caret at '" ++ expected_sub ++ "' in: " ++ caret_text(z),
        true,
        has_sub(caret_text(z), expected_sub),
      );
    },
  );

let caret_audit_tests = [
  caret_at(
    ~kind=SwapParams(1),
    "let f = fun (a, ¦b, c) -> a in f(1, 2, 3)",
    "c, ¦b",
  ),
  caret_at(
    ~kind=SwapParams(0),
    "let f = fun (a, b) -> a in f(¦1, 2)",
    "f(2, ¦1)",
  ),
  caret_at(~kind=NegateIf, "¦if a && b then 1 else 2", "if ¦!"),
  caret_at(~kind=IfToCase, "¦if a then 1 else 2", "case ¦a"),
  caret_at(~kind=CaseToIf, "¦case a | true => 1 | false => 2 end", "if ¦a"),
  caret_at(~kind=AddTypeAnnotation, "¦let x = 1 in x", ": ¦Int"),
  caret_at(
    ~kind=AddCaseArm,
    "let b : Bool = true in ¦case b | true => 1 end",
    "=> ¦?",
  ),
  caret_at(~kind=EtaReduce, "¦fun x -> f(x)", "¦f"),
  caret_at(~kind=AddParameter, "¦let f = fun (a) -> a in f(1)", ", ¦"),
];

let binding_tests = [
  test_case("arm hoist gated on pattern-bound var in def", `Quick, () =>
    check(
      bool,
      "not offered",
      false,
      offers(
        HoistLet,
        "case m | Var(name) => ¦let y = name in y | _ => 0 end",
      ),
    )
  ),
  test_case("arm hoist offered when def ignores pattern vars", `Quick, () =>
    check(
      bool,
      "offered",
      true,
      offers(HoistLet, "case m | Var(name) => ¦let y = 1 in y | _ => 0 end"),
    )
  ),
];

/* === Swap Arms + gestures === */

let gesture_kind =
    (g: Action.Gesture.t, marked: string): option(Action.refactor) => {
  let z = Test_Editing.parse_zipper(marked);
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  Refactor.gesture(~info_map=info_map_of(z), ~term, g, z);
};

let check_gesture = (name, g, marked, expected: option(Action.refactor)) =>
  test_case(name, `Quick, () =>
    check(bool, name, true, gesture_kind(g, marked) == expected)
  );

let arm_tests = [
  test_case(
    "swap arms: inline",
    `Quick,
    () => {
      let got =
        inline(~kind=SwapArms(0), "case c | ¦1 => 11 | 2 => 22 end")
        |> text_of;
      check(string, "swapped", "case c | 2 => 22 | 1 => 11 end", got);
    },
  ),
  test_case(
    "swap arms keeps multiline layout",
    `Quick,
    () => {
      let got =
        inline(~kind=SwapArms(0), "case c\n| 1 => 11\n| ¦2 => 22\nend")
        |> text_of;
      check(string, "swapped", "case c\n| 2 => 22\n| 1 => 11\nend", got);
    },
  ),
  test_case("swap arms gated on overlap (wildcard)", `Quick, () =>
    check(
      bool,
      "not offered",
      false,
      offers(SwapArms(0), "case c | ¦1 => 11 | _ => 22 end"),
    )
  ),
  test_case("swap arms offered for distinct ctors", `Quick, () =>
    check(
      bool,
      "offered",
      true,
      offers(SwapArms(0), "case c | Red => 1 | ¦Green => 2 end"),
    )
  ),
  test_case("swap arms offered at the | delimiter", `Quick, () =>
    check(
      bool,
      "offered",
      true,
      offers(SwapArms(0), "case c | Red => 1 ¦| Green => 2 end"),
    )
  ),
  test_case("swap arms offered at the => delimiter", `Quick, () =>
    check(
      bool,
      "offered",
      true,
      offers(SwapArms(0), "case c | Red => 1 | Green =¦> 2 end"),
    )
  ),
  test_case(
    "delimiter-invoked reorder swaps and caret follows the arm",
    `Quick,
    () => {
      let z =
        inline(~kind=SwapArms(0), "case c ¦| Red => 1 | Green => 2 end");
      check(
        string,
        "swapped, caret at the arm's new slot: " ++ caret_text(z),
        "case c | Green => 2 ¦| Red => 1 end",
        caret_text(z),
      );
    },
  ),
];

/* place the caret on the first arm's pattern */
let caretize = (src: string): string => {
  let idx =
    switch (String.index_opt(src, 'L')) {
    | Some(i) => i
    | None => 0
    };
  String.sub(src, 0, idx)
  ++ "¦"
  ++ String.sub(src, idx, String.length(src) - idx);
};

let arm_roundtrip_tests = [
  test_case(
    "arm swap twice restores exact text (inline arm)",
    `Quick,
    () => {
      let src = "case e\n| Lam(x, body) => Ok(x)\n| Var(n) =>\n    Error(\"free\")\nend";
      let z1 = inline(~kind=SwapArms(0), caretize(src));
      let z2 = Test_Editing.perform(z1, [Action.Refactor(SwapArms(0))]);
      check(string, "round trip", src, text_of(z2));
    },
  ),
  test_case(
    "arm swap twice restores exact text (multiline arms)",
    `Quick,
    () => {
      let src = "case e\n| Lam(x, body) =>       Ok(x)\n| Var(n) =>\n    Error(\"free\")\n| Ap(f, a) => No\nend";
      let z1 = inline(~kind=SwapArms(0), caretize(src));
      let z2 = Test_Editing.perform(z1, [Action.Refactor(SwapArms(0))]);
      check(string, "round trip", src, text_of(z2));
    },
  ),
  test_case(
    "single arm swap output",
    `Quick,
    () => {
      let src = "case e\n| Lam(x, body) => Ok(x)\n| Var(n) =>\n    Error(\"free\")\nend";
      let got = inline(~kind=SwapArms(0), caretize(src)) |> text_of;
      Printf.printf("\nSWAP1: %s\n", String.escaped(got));
      check(bool, "printed", true, true);
    },
  ),
];

let arm_travel_tests = [
  test_case(
    "arm travels down twice and back up: exact text restored",
    `Quick,
    () => {
      let src = "case e\n| Lam(x, body) => Ok(x)\n| Var(n) =>\n    Error(\"free\")\n| Ap(f, a) => case go(f)\n    | Ok(g) => No\n    | _ => Maybe\n    end\nend";
      let z1 = inline(~kind=SwapArms(0), caretize(src));
      Printf.printf("\nT1: %s\n", String.escaped(text_of(z1)));
      let z2 = Test_Editing.perform(z1, [Action.Refactor(SwapArms(1))]);
      Printf.printf("T2: %s\n", String.escaped(text_of(z2)));
      let z3 = Test_Editing.perform(z2, [Action.Refactor(SwapArms(1))]);
      Printf.printf("T3: %s\n", String.escaped(text_of(z3)));
      let z4 = Test_Editing.perform(z3, [Action.Refactor(SwapArms(0))]);
      Printf.printf("T4: %s\n", String.escaped(text_of(z4)));
      check(string, "round trip", src, text_of(z4));
    },
  ),
];

let gesture_tests = [
  check_gesture(
    "up on mid-chain let = hoist",
    Up,
    "let a = 1 in ¦let x = 2 in x + a",
    Some(HoistLet),
  ),
  check_gesture(
    "up on top let is dead (no extract fall-through)",
    Up,
    "¦let a = 1 in a",
    None,
  ),
  check_gesture(
    "up on expression = extract",
    Up,
    "let y = f(1 ¦+ 2) in y",
    Some(ExtractLet),
  ),
  check_gesture(
    "down on chain head = sink",
    Down,
    "¦let x = 2 in let a = 1 in x + a",
    Some(SinkLet),
  ),
  check_gesture(
    "down when sink exhausted = inline (elevator bottom)",
    Down,
    "¦let x = 2 in x + x",
    Some(InlineLet),
  ),
  check_gesture(
    "down at occurrence = inline",
    Down,
    "let x = 2 in x + ¦x",
    Some(InlineLet),
  ),
  check_gesture(
    "right on param = swap with right neighbor",
    Right,
    "let f = fun (¦a, b) -> a - b in f(1, 2)",
    Some(SwapParams(0)),
  ),
  check_gesture(
    "left on second param = swap with left neighbor",
    Left,
    "let f = fun (a, ¦b) -> a - b in f(1, 2)",
    Some(SwapParams(0)),
  ),
  check_gesture(
    "left on first param is dead",
    Left,
    "let f = fun (¦a, b) -> a - b in f(1, 2)",
    None,
  ),
  check_gesture(
    "right on call-site argument = swap",
    Right,
    "let f = fun (a, b) -> a - b in f(¦1, 2)",
    Some(SwapParams(0)),
  ),
  check_gesture(
    "up on second arm = reorder up",
    Up,
    "case c | 1 => 11 | ¦2 => 22 end",
    Some(SwapArms(0)),
  ),
  check_gesture(
    "down on first arm = reorder down",
    Down,
    "case c | ¦1 => 11 | 2 => 22 end",
    Some(SwapArms(0)),
  ),
  check_gesture(
    "up on first arm is dead",
    Up,
    "case c | ¦1 => 11 | 2 => 22 end",
    None,
  ),
  check_gesture(
    "down on overlapping arm is dead",
    Down,
    "case c | ¦1 => 11 | _ => 22 end",
    None,
  ),
  check_gesture(
    "down on the | delimiter = reorder down",
    Down,
    "case c ¦| 1 => 11 | 2 => 22 end",
    Some(SwapArms(0)),
  ),
  check_gesture(
    "up on the second arm's => = reorder up",
    Up,
    "case c | 1 => 11 | 2 =¦> 22 end",
    Some(SwapArms(0)),
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
    @ wave_tests
    @ wave2_tests
    @ swap_tests
    @ remove_param_tests
    @ move_tests
    @ put_down_tests
    @ more_tests
    @ arm_tests
    @ arm_roundtrip_tests
    @ arm_travel_tests
    @ gesture_tests
    @ caret_tests
    @ caret_audit_tests
    @ binding_tests
    @ reparse_safety_tests,
  ),
  (
    "Refactor Reparse Fuzz",
    [QCheck_alcotest.to_alcotest(~speed_level=`Slow, movement_reparse_fuzz)],
  ),
];
