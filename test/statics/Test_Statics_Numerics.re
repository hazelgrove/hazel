open Test_Statics_Prelude;
open Alcotest;
open Language;

let statics_no_disambiguate =
  Statics.mk(
    ~disambiguate_numerics=false,
    CoreSettings.on,
    Builtins.ctx_init(Some(Int)),
  );

let tests = (
  "Statics.Numerics",
  [
    test_case(
      "Disambiguation enabled: 1 : Float has no errors",
      `Quick,
      () => {
        let exp = parse_exp({|1 : Float|});
        let s = statics(exp);
        let errors = Statics.Map.errors(s) |> List.map(snd);
        Alcotest.check(list(testable_error), "no errors", [], errors);
      },
    ),
    test_case(
      "Disambiguation disabled: 1 : Float has errors",
      `Quick,
      () => {
        let exp = parse_exp({|1 : Float|});
        let s = statics_no_disambiguate(exp);
        let errors = Statics.Map.errors(s) |> List.map(snd);
        Alcotest.check(neg(list(testable_error)), "has errors", [], errors);
      },
    ),
    test_case(
      "Disambiguation disabled: 1.0 : Float has no errors",
      `Quick,
      () => {
        let exp = parse_exp({|1.0 : Float|});
        let s = statics_no_disambiguate(exp);
        let errors = Statics.Map.errors(s) |> List.map(snd);
        Alcotest.check(list(testable_error), "no errors", [], errors);
      },
    ),
  ],
);
