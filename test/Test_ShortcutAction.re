open Alcotest;
module A = Web.ShortcutAction;

/* The registry makes action NAMES correct by construction — the config and
   the palette both fold over `all`. These pin the two invariants that the
   type system still cannot enforce. */

/* The override table is keyed by label, and the config slide turns each
   label into a record field, so two actions sharing one would be ambiguous
   in both places. */
let labels_unique = () => {
  let labels = List.map(A.label, A.all);
  let dupes =
    List.filter(
      l => List.length(List.filter((==)(l), labels)) > 1,
      List.sort_uniq(compare, labels),
    );
  check(list(string), "no two actions share a label", [], dupes);
};

/* `all_sections` is a hand-written list; a section variant missing from it
   would silently drop every action in that section out of the config slide
   and its analyzed type, with nothing else to catch it. */
let every_action_reachable = () => {
  let reachable =
    List.concat_map(A.in_section, A.populated_sections) |> List.length;
  check(
    int,
    "every action appears under some section in all_sections",
    List.length(A.all),
    reachable,
  );
};

/* A program-dependent palette entry (of_dynamic) is never rebindable: it has
   no hotkey, and an override keyed by a coinciding label does not reach it. */
let dynamic_entries_stay_unbound = () => {
  let overrides = [(A.label(Undo), Some("ctrl+u"))];
  let dynamic =
    Web.ContextualAction.of_dynamic(
      ~action=Virtual_dom.Vdom.Effect.Ignore,
      ~section=Refactoring,
      ~mdIcon="compress",
      A.label(Undo),
    );
  check(option(string), "no hotkey of its own", None, dynamic.hotkey);
  check(
    option(string),
    "an override does not bind it",
    None,
    Web.NinjaKeys.apply_override(~overrides, dynamic).hotkey,
  );
  check(
    option(string),
    "a registry entry still takes the override",
    Some("ctrl+u"),
    Web.NinjaKeys.apply_override(
      ~overrides,
      Web.ContextualAction.of_shortcut(Undo),
    ).
      hotkey,
  );
};

let tests = [
  (
    "ShortcutAction",
    [
      test_case("labels are unique", `Quick, labels_unique),
      test_case(
        "all_sections reaches every action",
        `Quick,
        every_action_reachable,
      ),
      test_case(
        "dynamic entries stay unbound",
        `Quick,
        dynamic_entries_stay_unbound,
      ),
    ],
  ),
];
