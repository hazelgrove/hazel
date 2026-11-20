open Alcotest;
open Language;

module F = Web.FlagsConfiguration;

let builtin_source_satisfies_expected_type = () =>
  check(
    int,
    "built-in Flags source has no static errors under its expected type",
    0,
    ConfigSlideCheck.error_count(~ana=F.expected_type, F.source),
  );

let analysis_is_engaged = () =>
  check(
    bool,
    "analyzing the Flags source against String reports an error",
    true,
    ConfigSlideCheck.error_count(
      ~ana=IdTagged.FreshGrammar.Typ.string(),
      F.source,
    )
    > 0,
  );

let hud_is_off_by_default = () => {
  check(
    bool,
    "the built-in slide's value has hud = false",
    false,
    F.hud_of_value(
      ConfigSlideCheck.evaluate(~ana=F.expected_type, F.source),
    ),
  );
  check(bool, "the startup read agrees", false, F.hud_of_source(F.source));
};

let hud_reads_true = () =>
  check(
    bool,
    "(hud = true) turns the HUD on",
    true,
    F.hud_of_value(
      IdTagged.FreshGrammar.Exp.(
        tuple([tup_label(label(F.hud_label), bool(true))])
      ),
    ),
  );

let tests = [
  (
    "FlagsConfiguration",
    [
      test_case(
        "built-in source type-checks",
        `Quick,
        builtin_source_satisfies_expected_type,
      ),
      test_case("analysis is engaged", `Quick, analysis_is_engaged),
      test_case("the HUD is off by default", `Quick, hud_is_off_by_default),
      test_case("hud = true reads as on", `Quick, hud_reads_true),
    ],
  ),
];
