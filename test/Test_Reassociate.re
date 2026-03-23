open Alcotest;
open Haz3lcore;
open Action;

let deep_reassociate_settings = {
  ...Test_Editing.default_settings,
  deep_reassociate: true,
};

let deep_reassociate_tests = [
  test_case(
    "Parens wrap via out-of-order ( then )",
    `Quick,
    () => {
      let z =
        Test_Editing.mk("(1+¦2)")
        @ [Insert("(")]
        @ Test_Editing.mv_r(2)
        @ [Insert(")")]
        |> Test_Editing.perform(~settings=deep_reassociate_settings, Zipper.init());
      if (Test_Editing.zip_has_incomplete(z)) {
        Alcotest.fail(
          "Incomplete tiles remain — both parens pairs should be complete",
        );
      };
      let seg = Zipper.zip(z);
      let parens = Test_Editing.find_tiles_by_label(["(", ")"], seg);
      let complete_parens = List.filter(Tile.is_complete, parens);
      if (List.length(complete_parens) != 2) {
        Alcotest.fail(
          Printf.sprintf(
            "Expected 2 complete paren tiles, got %d",
            List.length(complete_parens),
          ),
        );
      };
    },
  ),
  test_case(
    "Inserting let between existing lets preserves outer",
    `Quick,
    () => {
      let z0 =
        Test_Editing.mk("let a = ¦ in a")
        |> Test_Editing.perform(
             ~settings=deep_reassociate_settings,
             Zipper.init(),
           );
      let count_complete_lets = (z: Zipper.t) => {
        let seg = Zipper.zip(z);
        let lets = Test_Editing.find_tiles_by_label(["let", "=", "in"], seg);
        List.length(List.filter(Tile.is_complete, lets));
      };
      let z1 =
        Test_Editing.string_to_ltr_actions("let ")
        |> Test_Editing.perform(~settings=deep_reassociate_settings, z0);
      if (count_complete_lets(z1) < 1) {
        Alcotest.fail(
          "After 'let ': outer let broken (expected >= 1 complete)",
        );
      };
      let z2 =
        Test_Editing.string_to_ltr_actions("x = ")
        |> Test_Editing.perform(~settings=deep_reassociate_settings, z1);
      if (count_complete_lets(z2) < 1) {
        Alcotest.fail(
          "After 'x = ': outer let broken (expected >= 1 complete)",
        );
      };
      let z3 =
        Test_Editing.string_to_ltr_actions("2 in ")
        |> Test_Editing.perform(~settings=deep_reassociate_settings, z2);
      if (count_complete_lets(z3) != 2) {
        Alcotest.fail(
          Printf.sprintf(
            "After '2 in ': expected 2 complete lets, got %d",
            count_complete_lets(z3),
          ),
        );
      };
    },
  ),
  test_case(
    "Typing let before trailing body preserves outer",
    `Quick,
    () => {
      let z0 =
        Test_Editing.mk("let a = 1 in¦ a")
        |> Test_Editing.perform(
             ~settings=deep_reassociate_settings,
             Zipper.init(),
           );
      let count_complete_lets = (z: Zipper.t) => {
        let seg = Zipper.zip(z);
        let lets = Test_Editing.find_tiles_by_label(["let", "=", "in"], seg);
        List.length(List.filter(Tile.is_complete, lets));
      };
      let z1 =
        Test_Editing.string_to_ltr_actions(" let b = ")
        |> Test_Editing.perform(~settings=deep_reassociate_settings, z0);
      if (count_complete_lets(z1) < 1) {
        Alcotest.fail(
          Printf.sprintf(
            "After ' let b = ': expected >= 1 complete let, got %d",
            count_complete_lets(z1),
          ),
        );
      };
      let z2 =
        Test_Editing.string_to_ltr_actions("2 in")
        |> Test_Editing.perform(~settings=deep_reassociate_settings, z1);
      if (count_complete_lets(z2) != 2) {
        Alcotest.fail(
          Printf.sprintf(
            "After '2 in': expected 2 complete lets, got %d",
            count_complete_lets(z2),
          ),
        );
      };
    },
  ),
  test_case(
    "Paste incomplete let before trailing body preserves outer",
    `Quick,
    () => {
      let z0 =
        Test_Editing.mk("let a = 1 in¦ a")
        |> Test_Editing.perform(
             ~settings=deep_reassociate_settings,
             Zipper.init(),
           );
      let count_complete_lets = (z: Zipper.t) => {
        let seg = Zipper.zip(z);
        let lets = Test_Editing.find_tiles_by_label(["let", "=", "in"], seg);
        List.length(List.filter(Tile.is_complete, lets));
      };
      let z1 =
        Test_Editing.perform(
          ~settings=deep_reassociate_settings,
          z0,
          [Paste(" let b = ")],
        );
      if (count_complete_lets(z1) < 1) {
        Alcotest.fail(
          Printf.sprintf(
            "After pasting ' let b = ': expected >= 1 complete let, got %d",
            count_complete_lets(z1),
          ),
        );
      };
      let z2 =
        Test_Editing.perform(
          ~settings=deep_reassociate_settings,
          z1,
          [Paste("2 in")],
        );
      if (count_complete_lets(z2) != 2) {
        Alcotest.fail(
          Printf.sprintf(
            "After pasting '2 in': expected 2 complete lets, got %d",
            count_complete_lets(z2),
          ),
        );
      };
    },
  ),
  test_case(
    "Typing let inside nested lets preserves repeated ancestors",
    `Quick,
    () => {
      let z0 =
        Test_Editing.mk("let a = 1 in let b = ¦ in b")
        |> Test_Editing.perform(
             ~settings=deep_reassociate_settings,
             Zipper.init(),
           );
      let count_complete_lets = (z: Zipper.t) => {
        let seg = Zipper.zip(z);
        let lets = Test_Editing.find_tiles_by_label(["let", "=", "in"], seg);
        List.length(List.filter(Tile.is_complete, lets));
      };
      let z1 =
        Test_Editing.string_to_ltr_actions("let c = ")
        |> Test_Editing.perform(~settings=deep_reassociate_settings, z0);
      if (count_complete_lets(z1) < 2) {
        Alcotest.fail(
          Printf.sprintf(
            "After 'let c = ': expected >= 2 complete lets, got %d",
            count_complete_lets(z1),
          ),
        );
      };
      let z2 =
        Test_Editing.string_to_ltr_actions("2 in ")
        |> Test_Editing.perform(~settings=deep_reassociate_settings, z1);
      if (count_complete_lets(z2) != 3) {
        Alcotest.fail(
          Printf.sprintf(
            "After '2 in ': expected 3 complete lets, got %d",
            count_complete_lets(z2),
          ),
        );
      };
    },
  ),
  test_case(
    "List literals out-of-order [ then ]",
    `Quick,
    () => {
      let z =
        Test_Editing.mk("[1+¦2]")
        @ [Insert("[")]
        @ Test_Editing.mv_r(2)
        @ [Insert("]")]
        |> Test_Editing.perform(~settings=deep_reassociate_settings, Zipper.init());
      if (Test_Editing.zip_has_incomplete(z)) {
        Alcotest.fail(
          "Incomplete tiles remain — both bracket pairs should be complete",
        );
      };
      let seg = Zipper.zip(z);
      let brackets = Test_Editing.find_tiles_by_label(["[", "]"], seg);
      let complete = List.filter(Tile.is_complete, brackets);
      if (List.length(complete) != 2) {
        Alcotest.fail(
          Printf.sprintf(
            "Expected 2 complete bracket tiles, got %d",
            List.length(complete),
          ),
        );
      };
    },
  ),
  test_case(
    "Typing if inside if preserves outer",
    `Quick,
    () => {
      let z0 =
        Test_Editing.mk("if true then ¦ else 0")
        |> Test_Editing.perform(
             ~settings=deep_reassociate_settings,
             Zipper.init(),
           );
      let count_complete_ifs = (z: Zipper.t) => {
        let seg = Zipper.zip(z);
        let ifs =
          Test_Editing.find_tiles_by_label(["if", "then", "else"], seg);
        List.length(List.filter(Tile.is_complete, ifs));
      };
      let z1 =
        Test_Editing.string_to_ltr_actions("if ")
        |> Test_Editing.perform(~settings=deep_reassociate_settings, z0);
      if (count_complete_ifs(z1) < 1) {
        Alcotest.fail("After 'if ': outer if broken");
      };
      let z2 =
        Test_Editing.string_to_ltr_actions("false then ")
        |> Test_Editing.perform(~settings=deep_reassociate_settings, z1);
      if (count_complete_ifs(z2) < 1) {
        Alcotest.fail("After 'if false then ': outer if broken");
      };
      let z3 =
        Test_Editing.string_to_ltr_actions("1 else ")
        |> Test_Editing.perform(~settings=deep_reassociate_settings, z2);
      if (count_complete_ifs(z3) != 2) {
        Alcotest.fail(
          Printf.sprintf(
            "After completing inner if: expected 2, got %d",
            count_complete_ifs(z3),
          ),
        );
      };
    },
  ),
  test_case(
    "Typing fun inside fun preserves outer",
    `Quick,
    () => {
      let z0 =
        Test_Editing.mk("fun x -> ¦")
        |> Test_Editing.perform(
             ~settings=deep_reassociate_settings,
             Zipper.init(),
           );
      let count_complete_funs = (z: Zipper.t) => {
        let seg = Zipper.zip(z);
        let funs = Test_Editing.find_tiles_by_label(["fun", "->"], seg);
        List.length(List.filter(Tile.is_complete, funs));
      };
      let z1 =
        Test_Editing.string_to_ltr_actions("fun ")
        |> Test_Editing.perform(~settings=deep_reassociate_settings, z0);
      if (count_complete_funs(z1) < 1) {
        Alcotest.fail("After 'fun ': outer fun broken");
      };
      let z2 =
        Test_Editing.string_to_ltr_actions("y -> 1")
        |> Test_Editing.perform(~settings=deep_reassociate_settings, z1);
      if (count_complete_funs(z2) != 2) {
        Alcotest.fail(
          Printf.sprintf(
            "After completing inner fun: expected 2, got %d",
            count_complete_funs(z2),
          ),
        );
      };
    },
  ),
  test_case(
    "Nested parens: ( inside preserves existing, ) completes new",
    `Quick,
    () => {
      let z1 =
        Test_Editing.mk("(1+(¦2))")
        @ [Insert("(")]
        |> Test_Editing.perform(~settings=deep_reassociate_settings, Zipper.init());
      let count_complete_parens = (z: Zipper.t) => {
        let seg = Zipper.zip(z);
        let ps = Test_Editing.find_tiles_by_label(["(", ")"], seg);
        List.length(List.filter(Tile.is_complete, ps));
      };
      if (count_complete_parens(z1) < 2) {
        Alcotest.fail(
          Printf.sprintf(
            "After inserting (: expected >= 2 complete parens, got %d",
            count_complete_parens(z1),
          ),
        );
      };
      let z2 =
        Test_Editing.mv_r(1)
        @ [Insert(")")]
        |> Test_Editing.perform(~settings=deep_reassociate_settings, z1);
      if (count_complete_parens(z2) != 3) {
        Alcotest.fail(
          Printf.sprintf(
            "After inserting ): expected 3 complete parens, got %d",
            count_complete_parens(z2),
          ),
        );
      };
    },
  ),
  test_case(
    "Typing let inside if preserves outer if",
    `Quick,
    () => {
      let z0 =
        Test_Editing.mk("if true then ¦ else 0")
        |> Test_Editing.perform(
             ~settings=deep_reassociate_settings,
             Zipper.init(),
           );
      let count_complete_ifs = (z: Zipper.t) => {
        let seg = Zipper.zip(z);
        let ifs =
          Test_Editing.find_tiles_by_label(["if", "then", "else"], seg);
        List.length(List.filter(Tile.is_complete, ifs));
      };
      let z1 =
        Test_Editing.string_to_ltr_actions("let x = ")
        |> Test_Editing.perform(~settings=deep_reassociate_settings, z0);
      if (count_complete_ifs(z1) < 1) {
        Alcotest.fail("After 'let x = ': outer if broken");
      };
      let z2 =
        Test_Editing.string_to_ltr_actions("1 in ")
        |> Test_Editing.perform(~settings=deep_reassociate_settings, z1);
      if (count_complete_ifs(z2) < 1) {
        Alcotest.fail("After completing let: outer if broken");
      };
      let count_complete_lets = (z: Zipper.t) => {
        let seg = Zipper.zip(z);
        let lets = Test_Editing.find_tiles_by_label(["let", "=", "in"], seg);
        List.length(List.filter(Tile.is_complete, lets));
      };
      if (count_complete_lets(z2) != 1) {
        Alcotest.fail(
          Printf.sprintf(
            "After completing let: expected 1 complete let, got %d",
            count_complete_lets(z2),
          ),
        );
      };
    },
  ),
  test_case(
    "Destruct inner in preserves outer let",
    `Quick,
    () => {
      let z =
        Test_Editing.mk("let a = 1 in let b = 2 in¦ b")
        @ [Destruct(Left), Destruct(Left)]
        |> Test_Editing.perform(~settings=deep_reassociate_settings, Zipper.init());
      let seg = Zipper.zip(z);
      let lets = Test_Editing.find_tiles_by_label(["let", "=", "in"], seg);
      let complete = List.filter(Tile.is_complete, lets);
      if (List.length(complete) < 1) {
        Alcotest.fail("Outer let broken after deleting inner in");
      };
    },
  ),
  test_case(
    "Paste completes if/then/else across ancestor scope",
    `Quick,
    () => {
      let z =
        Test_Editing.mk("if true ¦")
        |> Test_Editing.perform(
             ~settings=deep_reassociate_settings,
             Zipper.init(),
           );
      let z =
        Test_Editing.perform(
          ~settings=deep_reassociate_settings,
          z,
          [Paste("then 1 else 2")],
        );
      if (Test_Editing.zip_has_incomplete(z)) {
        Alcotest.fail(
          "Incomplete tiles after paste — then/else should match ancestor if",
        );
      };
      let seg = Zipper.zip(z);
      let ifs = Test_Editing.find_tiles_by_label(["if", "then", "else"], seg);
      let complete_ifs = List.filter(Tile.is_complete, ifs);
      if (List.length(complete_ifs) != 1) {
        Alcotest.fail(
          Printf.sprintf(
            "Expected 1 complete if/then/else, got %d",
            List.length(complete_ifs),
          ),
        );
      };
    },
  ),
  test_case(
    "Paste nested cross-scope delimiters (regression)",
    `Quick,
    () => {
      let z =
        Test_Editing.mk("if true ¦")
        |> Test_Editing.perform(
             ~settings=deep_reassociate_settings,
             Zipper.init(),
           );
      let z =
        Test_Editing.perform(
          ~settings=deep_reassociate_settings,
          z,
          [Paste("then mapi(fun _ -> if false then 1 else 2) else []")],
        );
      if (Test_Editing.zip_has_incomplete(z)) {
        Alcotest.fail(
          "Incomplete tiles after paste — nested cross-scope delimiters broken",
        );
      };
      let seg = Zipper.zip(z);
      let ifs = Test_Editing.find_tiles_by_label(["if", "then", "else"], seg);
      let complete_ifs = List.filter(Tile.is_complete, ifs);
      if (List.length(complete_ifs) != 2) {
        Alcotest.fail(
          Printf.sprintf(
            "Expected 2 complete if/then/else, got %d",
            List.length(complete_ifs),
          ),
        );
      };
      let funs = Test_Editing.find_tiles_by_label(["fun", "->"], seg);
      let complete_funs = List.filter(Tile.is_complete, funs);
      if (List.length(complete_funs) != 1) {
        Alcotest.fail(
          Printf.sprintf(
            "Expected 1 complete fun/->. got %d",
            List.length(complete_funs),
          ),
        );
      };
    },
  ),
  test_case(
    "Paste completes nested ifs with repeated ancestor labels",
    `Quick,
    () => {
      let z =
        Test_Editing.mk("if a then if b ¦")
        |> Test_Editing.perform(
             ~settings=deep_reassociate_settings,
             Zipper.init(),
           );
      let z =
        Test_Editing.perform(
          ~settings=deep_reassociate_settings,
          z,
          [Paste("then 1 else 2 else 3")],
        );
      if (Test_Editing.zip_has_incomplete(z)) {
        Alcotest.fail(
          "Incomplete tiles after paste — nested repeated-label ifs broken",
        );
      };
      let seg = Zipper.zip(z);
      let ifs = Test_Editing.find_tiles_by_label(["if", "then", "else"], seg);
      let complete_ifs = List.filter(Tile.is_complete, ifs);
      if (List.length(complete_ifs) != 2) {
        Alcotest.fail(
          Printf.sprintf(
            "Expected 2 complete nested if/then/else tiles, got %d",
            List.length(complete_ifs),
          ),
        );
      };
    },
  ),
  test_case(
    "Nested parens complete when ) is typed past multiple ancestors",
    `Quick,
    () => {
      let z =
        Test_Editing.mk("((1+¦2))")
        @ [Insert("(")]
        @ Test_Editing.mv_r(3)
        @ [Insert(")")]
        |> Test_Editing.perform(~settings=deep_reassociate_settings, Zipper.init());
      if (Test_Editing.zip_has_incomplete(z)) {
        Alcotest.fail(
          "Incomplete tiles remain — all nested parens should be complete",
        );
      };
      let seg = Zipper.zip(z);
      let parens = Test_Editing.find_tiles_by_label(["(", ")"], seg);
      let complete_parens = List.filter(Tile.is_complete, parens);
      if (List.length(complete_parens) != 3) {
        Alcotest.fail(
          Printf.sprintf(
            "Expected 3 complete paren tiles, got %d",
            List.length(complete_parens),
          ),
        );
      };
    },
  ),
  test_case(
    "Nested case/test complete when end is typed past ancestor",
    `Quick,
    () => {
      let z =
        Test_Editing.mk("case x | A => ¦1 end")
        @ Test_Editing.string_to_ltr_actions("test ")
        @ Test_Editing.mv_r(5)
        @ Test_Editing.string_to_ltr_actions(" end")
        |> Test_Editing.perform(~settings=deep_reassociate_settings, Zipper.init());
      if (Test_Editing.zip_has_incomplete(z)) {
        Alcotest.fail(
          "Incomplete tiles remain — both nested end-delimited forms should be complete",
        );
      };
      let seg = Zipper.zip(z);
      let cases = Test_Editing.find_tiles_by_label(["case", "end"], seg);
      let complete_cases = List.filter(Tile.is_complete, cases);
      if (List.length(complete_cases) != 1) {
        Alcotest.fail(
          Printf.sprintf(
            "Expected 1 complete case/end tile, got %d",
            List.length(complete_cases),
          ),
        );
      };
      let tests = Test_Editing.find_tiles_by_label(["test", "end"], seg);
      let complete_tests = List.filter(Tile.is_complete, tests);
      if (List.length(complete_tests) != 1) {
        Alcotest.fail(
          Printf.sprintf(
            "Expected 1 complete test/end tile, got %d",
            List.length(complete_tests),
          ),
        );
      };
    },
  ),
];

let tests = [
  ("Editing.DeepReassociate", deep_reassociate_tests),
];
