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

let inline = (marked: string): Zipper.t => {
  let z = Test_Editing.parse_zipper(marked);
  Test_Editing.perform(z, [Action.Refactor(InlineLet)]);
};

let applicable_at = (marked: string): bool =>
  Refactor.applicable(Test_Editing.parse_zipper(marked));

let gating_tests = [
  test_case("caret before let", `Quick, () =>
    check(bool, "a", true, applicable_at("¦let x = 1 in x"))
  ),
  test_case("caret inside let kw", `Quick, () =>
    check(bool, "b", true, applicable_at("l¦et x = 1 in x"))
  ),
  test_case("caret after let kw", `Quick, () =>
    check(bool, "c", true, applicable_at("let¦ x = 1 in x"))
  ),
  test_case("caret at eq", `Quick, () =>
    check(bool, "d", true, applicable_at("let x =¦ 1 in x"))
  ),
  test_case("caret on body var", `Quick, () =>
    check(bool, "e", false, applicable_at("let x = 1 in ¦x"))
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
    "non-var pattern is not applicable",
    `Quick,
    () => {
      let z = Test_Editing.parse_zipper("¦let (a, b) = p in a");
      check(bool, "applicable is false", false, Refactor.applicable(z));
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

let tests = [("Refactor", refactor_tests @ gating_tests)];
