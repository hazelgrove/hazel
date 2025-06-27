/**
 * This file contains tests to validate the `MakeTerm` module's ability to convert
 * zippers into expressions.
 */
open Alcotest;
open Haz3lcore;
module Fresh = Language.IdTagged.FreshGrammar;

let zipper_testable =
  testable(Fmt.using(Zipper.show, Fmt.string), Zipper.equal);

let syntax_string = testable(Fmt.string, String.equal);

let parse_zipper = (s: string) => {
  switch (Printer.zipper_of_string(s)) {
  | Some(zip) => zip
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};

let perform = (zip: Zipper.t, actions: list(Action.t)): Zipper.t => {
  /* This is a simplified testing harness for zipper actions.
   * It does not incorporate any semantics; purely syntatic actions. */
  let mk_syntax: Zipper.t => Editor.CachedSyntax.t =
    Editor.CachedSyntax.init(
      ~info_map=Language.Statics.Map.empty,
      ~dyn_map=Language.Dynamics.Map.empty,
    );
  let mk_state: Zipper.t => Editor.State.t =
    z => {
      zipper: z,
      col_target: None,
    };
  let mk_move = (z: Zipper.t): (module Move.S) =>
    Editor.Model.to_move_s({
      state: mk_state(z),
      syntax: mk_syntax(z),
    });
  let perform = (a: Action.t, z: Zipper.t) =>
    Perform.go_z(
      ~settings=Language.CoreSettings.off,
      CachedStatics.empty,
      a,
      mk_move(z),
      z,
    );
  List.fold_left(
    (z: Zipper.t, a: Action.t) =>
      switch (perform(a, z)) {
      | Ok(z) => z
      | Error(err) =>
        print_endline(Zipper.show(z));
        Alcotest.fail(
          "Failed to perform action: " ++ Action.Failure.show(err),
        );
      },
    zip,
    actions,
  );
};

let find_tilde = (str: string): (int, string) => {
  let idx = String.index(str, '~');
  let without_tilde =
    String.sub(str, 0, idx)
    ++ String.sub(str, idx + 1, String.length(str) - idx - 1);
  (idx, without_tilde);
};

let zip_check_with_caret =
    ((init_with_tilde: string, actions: list(Action.t)), actual) => {
  /* This harness uses a tilde to represent caret position.
   * This assumes there are no syntactic tildes preceeding
   * the caret tilde in the syntax.  */
  let (caret_index, init_without_tilde) = find_tilde(init_with_tilde);

  /* NOTE: The first action (move to start) will fail if the syntax
   * is empty, as that means we're already at the start */
  let movement_actions: list(Action.t) =
    List.cons(
      Action.Move(Extreme(Left(ByToken))),
      List.init(caret_index, (_: int) =>
        Action.Move(Local(Right(ByChar)))
      ),
    );

  // Combine movement actions with the given actions
  let all_actions = List.append(movement_actions, actions);

  let printer =
    Printer.zipper_to_string(~holes=Some("?"), ~concave_holes=Some(">?<"));

  check(
    syntax_string,
    actual,
    perform(parse_zipper(init_without_tilde), all_actions) |> printer,
    parse_zipper(actual) |> printer,
  );
};

let tests = (
  "Editing",
  [
    test_case("Insert char at end", `Quick, () =>
      zip_check_with_caret(("fo~", [Insert("o")]), "foo")
    ),
    test_case("Insert char inside token", `Quick, () =>
      zip_check_with_caret(("fi~me", [Insert("x")]), "fixme")
    ),
    /* When you insert an `i` here, it will be treated as a variable
       reference, not the beginning of the `in` yet. So a concave grout
       must be inserted; we make this consume the preceeding space if
       any to avoid jutter */
    test_case("Grout transmutation 1", `Quick, () =>
      zip_check_with_caret(("let a = 1 ~", [Insert("i")]), "let a = 1>?<i")
    ),
    /* When you drop the n, the concave grout that appears in the above
       case should disappear, leaving a space */
    test_case("Grout transmutation 2", `Quick, () =>
      zip_check_with_caret(
        ("let a = 1 i~", [Insert("n")]),
        "let a = 1 in?",
      )
    ),
    /* Expansion of empty list into list with hole, with no extra space */
    test_case("`[~]` Insert space", `Quick, () =>
      zip_check_with_caret(("[~]", [Insert(" ")]), "[?]")
    ),
    /* Expansion of `caseend` into empty case, with no extra space */
    test_case("`case~end` Insert space", `Quick, () =>
      zip_check_with_caret(("case~end", [Insert(" ")]), "case?end")
    ),
    /* Spliting tokens with no extra space */
    test_case("`1~1` Insert space", `Quick, () =>
      zip_check_with_caret(("1~1", [Insert(" ")]), "1>?<1")
    ),
    /* Spliting tokens when the latter must drop from backpack */
    test_case("`if~then` Insert space", `Quick, () =>
      zip_check_with_caret(("if~then", [Insert(" ")]), "if?then?")
    ),
    test_case("`if true~then` Insert space", `Quick, () =>
      zip_check_with_caret(
        ("if true~then", [Insert(" ")]),
        "if true then?",
      )
    ),
    test_case("`if~then` Insert parens", `Quick, () =>
      zip_check_with_caret(("if~then", [Insert("(")]), "if(?then?")
    ),
    /* Spliting tokens when both must expand */
    test_case("`if~if` Insert +", `Quick, () =>
      zip_check_with_caret(("if~if", [Insert("+")]), "if?+if?")
    ),
    /* Below test is slightly precious. Can't directly write
     * `if then~else` as then will instantly expand, so need
     * to do this indirectly.  */
    test_case("`if then~else` Insert space", `Quick, () =>
      zip_check_with_caret(
        ("if the~else", [Insert("n"), Insert(" ")]),
        "if then?else?",
      )
    ),
  ],
);
