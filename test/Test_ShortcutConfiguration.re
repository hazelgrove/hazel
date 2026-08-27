open Alcotest;
open Haz3lcore;
open Language;

/* The Shortcuts config slide is analyzed against
   ShortcutConfiguration.expected_type, wired up in
   ConfigurationMode.Model.expected_type and threaded to statics as ~ana.

   Two checks, because either alone is weak: the first pins that the built-in
   source actually satisfies the type it is checked against (so adding a
   shortcut to DefaultConfiguration without the type following along is a test
   failure, not a red config buffer); the second pins that the analysis is
   engaged at all, since a vacuous ~ana would pass the first on its own. */

let shortcuts_source_zipper = (): Zipper.t =>
  PersistentZipper.unpersist(Web.ShortcutConfiguration.source, ~root=Exp);

let error_count = (~ana: Typ.t, z: Zipper.t): int => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let (info_map, _) =
    Statics.mk(~ana, CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  Statics.Map.error_ids(info_map) |> List.length;
};

let builtin_source_satisfies_expected_type = () =>
  check(
    int,
    "built-in Shortcuts source has no static errors under its expected type",
    0,
    error_count(
      ~ana=Web.ShortcutConfiguration.expected_type,
      shortcuts_source_zipper(),
    ),
  );

/* Negative control: the same source against a type it cannot have. */
let analysis_is_engaged = () =>
  check(
    bool,
    "analyzing the Shortcuts source against String reports an error",
    true,
    error_count(
      ~ana=IdTagged.FreshGrammar.Typ.string(),
      shortcuts_source_zipper(),
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
