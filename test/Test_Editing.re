/**
 * This file contains tests to validate the `MakeTerm` module's ability to convert
 * zippers into expressions.
 */
open Util;
open Alcotest;
open Haz3lcore;
module Fresh = Language.IdTagged.FreshGrammar;

/* The following special characters are used in the tests to represent
 * grout and the caret. I'd like to use extended ascii/unicode chars
 * to avoid collisions (and be prettier) but some of the below logic
 * seems to choke on fancy chars... */

let caret_char = "¦"; /* Note this is two bytes */
let convex_char = "?";
let concave_char = "~";

let printer = (z: Zipper.t): string =>
  Printer.of_zipper(
    ~holes=convex_char,
    ~concave_holes=concave_char,
    ~caret=caret_char,
    z,
  );

let perform = (zip: Zipper.t, actions: list(Action.t)): Zipper.t => {
  /* This is a simplified testing harness for zipper actions.
   * It does not apply any semantics-based behaviors. */
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
        print_endline("Zipper: " ++ Zipper.show(z));
        Alcotest.fail("Failed on action: " ++ Action.Failure.show(err));
      },
    zip,
    actions,
  );
};

let string_to_ltr_actions = (s: string): list(Action.t) =>
  s |> Util.StringUtil.to_list |> List.map(c => Action.Insert(c));

let move_by_char_left_actions = (n: int): list(Action.t) =>
  List.init(n, _ => Action.Move(Local(Left(ByChar))));

let mk = (init: string): list(Action.t) => {
  /* This harness uses a tilde to represent caret position.
   * This assumes there are no literal instances of the caret
   * char proceeding the caret tilde in the syntax. This creates
   * a list of actions intended to insert the init string into the
   * zipper character-by-character, except for the caret character,
   * and then move left character by character until the indicated
   * caret position is reached */

  let caret_regexp = StringUtil.regexp(caret_char);
  let caret_index =
    switch (StringUtil.search(caret_regexp, init, 0)) {
    | Some((idx, _)) => idx
    | None => Alcotest.fail("Failed to find caret in: " ++ init)
    };
  let init_without_caret = StringUtil.replace(caret_regexp, init, "");

  /* After inserting all characters, we need to move left by the number
   * of characters that come after the caret position */
  let chars_after_caret = String.length(init_without_caret) - caret_index;

  string_to_ltr_actions(init_without_caret)
  @ move_by_char_left_actions(chars_after_caret);
};

let test = (~name, ~acts, ~goal): test_case(_) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      goal,
      goal,
      acts |> perform(Zipper.init()) |> printer,
    )
  );

let basic_tests = [
  test(
    ~name="Initialize caret position from string",
    ~acts=mk("¦foo"),
    ~goal="¦foo",
  ),
];

let insertion_tests = [
  /* INSERTION : BASIC*/
  test(
    ~name="Insert char at end of token",
    ~acts=mk({|fo¦|}) @ [Insert("o")],
    ~goal={|foo¦|},
  ),
  test(
    ~name="Insert char at start of token",
    ~acts=mk({|¦oo|}) @ [Insert("f")],
    ~goal={|f¦oo|},
  ),
  test(
    ~name="Insert char inside token",
    ~acts=mk({|fi¦me|}) @ [Insert("x")],
    ~goal={|fix¦me|},
  ),
  test(
    ~name="Inserting string quote inserts closing quote as well",
    ~acts=mk({|¦|}) @ [Insert("\"")],
    ~goal={|"¦"|},
  ),
  /* INSERTION: GROUT/SPACE TRANSMUTATION */
  /* When you insert an `i` here, it will be treated as a variable
     reference, not the beginning of the `in` yet. So a concave grout
     must be inserted; we make this consume the preceeding space if
     any to avoid jutter */
  test(
    ~name="Grout transmutation 1: Space to Concave Grout",
    ~acts=mk({|let a = 1 ¦|}) @ [Insert("i")],
    ~goal={|let a = 1~i¦|},
  ),
  /* Then, when you drop the n, the concave grout that appears in the
     above case should disappear, leaving a space */
  test(
    ~name="Grout transmutation 2: Concave Grout to Space",
    ~acts=mk({|let a = 1 i¦|}) @ [Insert("n")],
    ~goal={|let a = 1 in¦?|},
  ),
  /* INSERTION: TOKEN SPLITTING */
  test(
    ~name="`Split empty list",
    ~acts=mk({|[¦]|}) @ [Insert(" ")],
    ~goal={|[?¦]|},
  ),
  test(
    ~name="Split case end",
    ~acts=mk({|case¦end|}) @ [Insert(" ")],
    ~goal={|case?¦end|},
  ),
  test(
    ~name="`Split number literal",
    ~acts=mk({|1¦1|}) @ [Insert(" ")],
    ~goal={|1~¦1|},
  ),
  /* Spliting tokens when the latter must drop from backpack */
  test(
    ~name="Split 1st and 2nd delims of 3-delim form with space",
    ~acts=mk({|if¦then|}) @ [Insert(" ")],
    ~goal={|if?¦then?|},
  ),
  test(
    ~name="`Split mono child and 2nd delim of 3-delim form",
    ~acts=mk({|if true¦then|}) @ [Insert(" ")],
    ~goal={|if true ¦then?|},
  ),
  test(
    ~name="Split 1st and 2nd delims of 3-delim form with instant expander",
    ~acts=mk({|if¦then|}) @ [Insert("(")],
    ~goal={|if(¦?then?|},
  ),
  /* Spliting tokens when both must expand */
  test(
    ~name="Split two leading delated expander delims with bin op",
    ~acts=mk({|if¦if|}) @ [Insert("+")],
    ~goal={|if?+¦if?|},
  ),
  /* Below test is slightly precious. Can't directly write
     `if then¦else` as then will instantly expand, so need
     to do this indirectly. The space after the first hole
     isn't perfect but it'll do for now */
  test(
    ~name="Split 2nd delim of 3-delim form with space",
    ~acts=mk({|if the¦else|}) @ [Insert("n"), Insert(" ")],
    ~goal={|if? then?¦else?|},
  ),
];

let destruct_tests = [
  /* DESTRUCTION: BASIC */
  test(
    ~name="Delete char from token by backspacing",
    ~acts=mk({|f¦oo|}) @ [Destruct(Left)],
    ~goal={|¦oo|},
  ),
  test(
    ~name="Deleting string delimiter deletes string",
    ~acts=mk({|"¦"|}) @ [Destruct(Left)],
    ~goal={|¦?|},
  ),
  /* DESTRUCTION: TOKEN MERGING */
  test(
    ~name="`Merge to empty list by backspacing",
    ~acts=mk({|[1¦]|}) @ [Destruct(Left)],
    ~goal={|[¦]|},
  ),
  test(
    ~name="`Merge to empty tuple by deleting",
    ~acts=mk({|(¦1)|}) @ [Destruct(Right)],
    ~goal={|(¦)|},
  ),
  test(
    ~name="`Merge number literals across bin op by backspacing",
    ~acts=mk({|1+¦1|}) @ [Destruct(Left)],
    ~goal={|1¦1|},
  ),
];

let tests = [
  ("Editing.Basic", basic_tests),
  ("Editing.Insertion", insertion_tests),
  ("Editing.Destruction", destruct_tests),
];
