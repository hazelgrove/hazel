open Alcotest;
open Language;

/* The Shortcuts config slide is analyzed against
   ShortcutConfiguration.expected_type, wired up in
   ConfigurationMode.Model.expected_type and threaded to statics as ~ana.

   Two checks, because either alone is weak: the first pins that the built-in
   source actually satisfies the type it is checked against (so adding a
   shortcut to DefaultConfiguration without the type following along is a test
   failure, not a red config buffer); the second pins that the analysis is
   engaged at all, since a vacuous ~ana would pass the first on its own. */

let builtin_source_satisfies_expected_type = () =>
  check(
    int,
    "built-in Shortcuts source has no static errors under its expected type",
    0,
    ConfigSlideCheck.error_count(
      ~ana=Web.ShortcutConfiguration.expected_type,
      Web.ShortcutConfiguration.source,
    ),
  );

/* Negative control: the same source against a type it cannot have. */
let analysis_is_engaged = () =>
  check(
    bool,
    "analyzing the Shortcuts source against String reports an error",
    true,
    ConfigSlideCheck.error_count(
      ~ana=IdTagged.FreshGrammar.Typ.string(),
      Web.ShortcutConfiguration.source,
    )
    > 0,
  );

let tests = [
  (
    "ShortcutConfiguration.expected_type",
    [
      test_case(
        "built-in source type-checks",
        `Quick,
        builtin_source_satisfies_expected_type,
      ),
      test_case("analysis is engaged", `Quick, analysis_is_engaged),
    ],
  ),
];
