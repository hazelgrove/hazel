/* InputEvent.actions_of_input replays a change in the hidden text input's
 * value as editor actions: the path virtual keyboards and IMEs type through. */
open Alcotest;
open Haz3lcore;

let action = testable(Action.pp, Action.equal);

let actions = (~composed="", value) =>
  Web.InputEvent.actions_of_input(~composed, value);

let insert = s => Action.Insert(s);
let delete = Action.Destruct(Local(Left, ByChar));

let tests = [
  (
    "InputEvent.actions_of_input",
    [
      test_case("an unchanged value is a no-op", `Quick, () =>
        check(list(action), "let", [], actions(~composed="let", "let"))
      ),
      test_case(
        "appended text is one insert per grapheme",
        `Quick,
        () => {
          check(list(action), "a", [insert("a")], actions("a"));
          check(
            list(action),
            "let",
            [insert("l"), insert("e"), insert("t")],
            actions("let"),
          );
          check(
            list(action),
            "le -> let",
            [insert("t")],
            actions(~composed="le", "let"),
          );
        },
      ),
      test_case(
        "removed text is one delete per grapheme",
        `Quick,
        () => {
          check(
            list(action),
            "let -> le",
            [delete],
            actions(~composed="let", "le"),
          );
          check(
            list(action),
            "let -> empty",
            [delete, delete, delete],
            actions(~composed="let", ""),
          );
        },
      ),
      test_case("a rewritten tail is deleted then reinserted", `Quick, () =>
        check(
          list(action),
          "teh -> the",
          [delete, delete, insert("h"), insert("e")],
          actions(~composed="teh", "the"),
        )
      ),
      test_case(
        "a multi-codepoint grapheme is one insert",
        `Quick,
        () => {
          let thumbs_up_medium_skin = "👍🏽";
          check(
            list(action),
            thumbs_up_medium_skin,
            [insert(thumbs_up_medium_skin)],
            actions(thumbs_up_medium_skin),
          );
        },
      ),
      test_case("a newline inserts the linebreak token", `Quick, () =>
        check(
          list(action),
          "newline",
          [insert(Token.linebreak)],
          actions("\n"),
        )
      ),
      test_case("tabs are dropped", `Quick, () =>
        check(
          list(action),
          "a<tab>b",
          [insert("a"), insert("b")],
          actions("a\tb"),
        )
      ),
    ],
  ),
];
