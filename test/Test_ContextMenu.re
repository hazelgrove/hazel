/* ContextMenu rows that a phone depends on: "Put down", offered exactly
 * when the backpack can be dropped at the caret (a phone has no Tab key),
 * and the selection rows, which grow and shrink a selection without a
 * drag or Shift+arrows. */
open Alcotest;
open Haz3lcore;

let zipper = (acts: list(Action.t)): Zipper.t =>
  Test_Editing.perform(Zipper.init(), acts);

let labels = (items: list(Util.Menu.item(_))): list(string) =>
  items
  |> List.filter_map(
       fun
       | Util.Menu.Action({label, _}) => Some(label)
       | Submenu(_)
       | Inline(_)
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
          labels(
            Web.ContextMenu.put_down_data(
              zipper(Test_Editing.mk("let x = 1 in¦ x")),
            ),
          ),
        )
      ),
      test_case("offered once a delimiter is picked up", `Quick, () =>
        check(
          list(string),
          "in deleted",
          ["Put down"],
          labels(
            Web.ContextMenu.put_down_data(
              zipper(
                Test_Editing.mk("let x = 1 in¦ x")
                @ [Destruct(Local(Left, ByChar))],
              ),
            ),
          ),
        )
      ),
    ],
  ),
  (
    "ContextMenu.selection_data",
    {
      let selection_labels = (~can_shrink, acts) =>
        labels(Web.ContextMenu.selection_data(~can_shrink, zipper(acts)));
      let selected =
        Test_Editing.mk("let x = ¦1 in x")
        @ [Select(Resize(Local(Right, ByChar)))];
      [
        test_case("selects the term at a bare caret", `Quick, () =>
          check(
            list(string),
            "caret",
            ["Select term"],
            selection_labels(
              ~can_shrink=false,
              Test_Editing.mk("let x = ¦1 in x"),
            ),
          )
        ),
        test_case("expands a selection", `Quick, () =>
          check(
            list(string),
            "selection",
            ["Expand selection"],
            selection_labels(~can_shrink=false, selected),
          )
        ),
        test_case("shrinks a selection it expanded", `Quick, () =>
          check(
            list(string),
            "expanded selection",
            ["Expand selection", "Shrink selection"],
            selection_labels(~can_shrink=true, selected),
          )
        ),
      ];
    },
  ),
];
