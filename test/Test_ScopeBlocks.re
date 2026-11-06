open Alcotest;
open Language;

let statics = (s: string): Statics.Map.t =>
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) =>
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), e) |> fst
  | None => fail("Failed to parse: " ++ s)
  };

let blocks = (s: string): list(Blocks.t) => ScopeBlocks.of_map(statics(s));

let paint = testable(Blocks.pp_paint, Blocks.equal_paint);
let shapes = list(pair(int, paint));

/* (level, paint) of every block in a fixed order: ids are random. */
let shape = (bs: list(Blocks.t)): list((int, Blocks.paint)) =>
  bs
  |> List.map((b: Blocks.t) => (b.level, b.paint))
  |> List.sort(((l1, p1), (l2, p2)) =>
       switch (Int.compare(l1, l2)) {
       | 0 => String.compare(Blocks.paint_name(p1), Blocks.paint_name(p2))
       | c => c
       }
     );

/* (level, scope depth) */
let check_shape = (program: string, expected: list((int, int))) =>
  check(
    shapes,
    program,
    List.map(((l, d)) => (l, Blocks.Scope(d)), expected),
    shape(blocks(program)),
  );

let test_let =
  test_case(
    "of_map: a let's body is a scope; its binder and use are painted with it",
    `Quick,
    () =>
    check_shape("let x = 1 in x + 1", [(0, 0), (0, 0), (1, 0)])
  );

let test_use_is_body =
  test_case(
    "of_map: a use that is the whole body is one deeper than the scope",
    `Quick,
    () =>
    check_shape("let x = 1 in x", [(0, 0), (0, 0), (1, 0)])
  );

let test_no_binder =
  test_case("of_map: a let binding no variable opens no scope", `Quick, () =>
    check_shape("let _ = 1 in 2", [])
  );

let test_unbound =
  test_case("of_map: an unbound use is not painted", `Quick, () =>
    check_shape("x", [])
  );

let test_outer_use =
  test_case(
    "of_map: a use inside an inner scope is painted by the scope that binds it",
    `Quick,
    () =>
    check_shape(
      "let x = 1 in let y = 2 in x",
      [(0, 0), (0, 0), (1, 1), (1, 1), (2, 0)],
    )
  );

let test_shadowing =
  test_case(
    "of_map: a shadowing use is painted by the inner scope", `Quick, () =>
    check_shape(
      "let x = 1 in let x = 2 in x",
      [(0, 0), (0, 0), (1, 1), (1, 1), (2, 1)],
    )
  );

let test_fun =
  test_case("of_map: a function's body is a scope", `Quick, () =>
    check_shape("fun x -> x", [(0, 0), (0, 0), (1, 0)])
  );

let test_rules =
  test_case(
    "of_map: a case rule's right-hand side is a scope when its pattern binds",
    `Quick,
    () =>
    check_shape("case 1 | y => y | _ => 0 end", [(0, 0), (0, 0), (1, 0)])
  );

/* The scope is the whole let: its definition uses the name, and the binder
   sits inside it. */
let test_recursive_let =
  test_case(
    "of_map: a recursive let is in scope of its own definition", `Quick, () =>
    check_shape(
      "let f = fun n -> f(n) in f(1)",
      [(0, 0), (1, 0), (1, 0), (1, 1), (1, 1), (2, 0), (2, 1)],
    )
  );

let test_function_sugar =
  test_case(
    "of_map: function sugar binds like the let it stands for", `Quick, () =>
    check_shape(
      "let f(n) = f(n) in f(1)",
      [(0, 0), (1, 0), (1, 0), (1, 1), (1, 1), (2, 0), (2, 1)],
    )
  );

let test_sorted_by_level =
  test_case(
    "of_map: blocks come in ascending level",
    `Quick,
    () => {
      let levels =
        blocks("let x = 1 in let y = 2 in x")
        |> List.map((b: Blocks.t) => b.level);
      check(list(int), "levels", List.sort(Int.compare, levels), levels);
    },
  );

let tests = (
  "ScopeBlocks",
  [
    test_let,
    test_use_is_body,
    test_no_binder,
    test_unbound,
    test_outer_use,
    test_shadowing,
    test_fun,
    test_rules,
    test_recursive_let,
    test_function_sugar,
    test_sorted_by_level,
  ],
);
