/* ContextMenu's "Put down" row: offered exactly when the backpack can be
 * dropped at the caret, since a phone has no Tab key to drop it with. */
open Alcotest;
open Haz3lcore;

let zipper = (acts: list(Action.t)): Zipper.t =>
  Test_Editing.perform(Zipper.init(), acts);

let labels = (z: Zipper.t): list(string) =>
  Web.ContextMenu.put_down_data(z)
  |> List.filter_map(
       fun
       | Util.Menu.Action({label, _}) => Some(label)
       | Submenu(_)
       | Divider => None,
     );

let tests = [
  (
    "ContextMenu.put_down_data",
    [
      test_case("absent while the backpack is empty", `Quick, () =>
        check(
          list(string),
          "let x = 1 in x",
          [],
          labels(zipper(Test_Editing.mk("let x = 1 in¦ x"))),
        )
      ),
      test_case("offered once a delimiter is picked up", `Quick, () =>
        check(
          list(string),
          "in deleted",
          ["Put down"],
          labels(
            zipper(
              Test_Editing.mk("let x = 1 in¦ x")
              @ [Destruct(Local(Left, ByChar))],
            ),
          ),
        )
      ),
    ],
  ),
];
