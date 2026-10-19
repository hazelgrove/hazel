open Alcotest;
open Language;

let statics = (s: string): Statics.Map.t =>
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) =>
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), e) |> fst
  | None => fail("Failed to parse: " ++ s)
  };

let blocks = (s: string): list(TypeBlocks.block) =>
  TypeBlocks.of_map(statics(s));

let head = testable(Typ.pp_head, Typ.equal_head);
let shapes = list(pair(int, head));

/* (level, head) of every block, in a fixed order: ids are random, so
   same-level blocks are ordered by head name. */
let shape = (bs: list(TypeBlocks.block)): list((int, Typ.head)) =>
  bs
  |> List.map((b: TypeBlocks.block) => (b.level, b.head))
  |> List.sort(((l1, h1), (l2, h2)) =>
       switch (Int.compare(l1, l2)) {
       | 0 => String.compare(Typ.head_name(h1), Typ.head_name(h2))
       | c => c
       }
     );

let check_shape = (program: string, expected: list((int, Typ.head))) =>
  check(shapes, program, expected, shape(blocks(program)));

let test_same_type_merges =
  test_case("of_map: same-typed subterms share one block", `Quick, () =>
    check_shape("1 + 2 * 3", [(0, Int)])
  );

let test_nested_heads =
  test_case("of_map: a head change nests a block one level deeper", `Quick, () =>
    check_shape(
      "[fun x -> x + 1]",
      [(0, List), (1, Arrow), (2, Int), (2, Unknown)],
    )
  );

let test_annotated_binder =
  test_case("of_map: an annotated binder is typed, not unknown", `Quick, () =>
    check_shape(
      "[fun x : Int -> x + 1]",
      [(0, List), (1, Arrow), (2, Int), (2, Int)],
    )
  );

let test_alias_resolves =
  test_case("of_map: type aliases resolve to their head", `Quick, () =>
    check_shape("type T = Int in let y : T = 1 in y", [(0, Int)])
  );

let test_fixed_type =
  test_case(
    "of_map: blocks follow the type after hole fixing, so an ill-typed operand joins its expected block",
    `Quick,
    () =>
    check_shape("1 + true", [(0, Int)])
  );

let test_hole_is_unknown =
  test_case("of_map: a hole is an unknown block", `Quick, () =>
    check_shape("?", [(0, Unknown)])
  );

let test_sorted_by_level =
  test_case(
    "of_map: blocks come in ascending level",
    `Quick,
    () => {
      let levels =
        blocks("[fun x -> x + 1]")
        |> List.map((b: TypeBlocks.block) => b.level);
      check(list(int), "levels", List.sort(Int.compare, levels), levels);
    },
  );

let tests = (
  "TypeBlocks",
  [
    test_same_type_merges,
    test_nested_heads,
    test_annotated_binder,
    test_alias_resolves,
    test_fixed_type,
    test_hole_is_unknown,
    test_sorted_by_level,
  ],
);
