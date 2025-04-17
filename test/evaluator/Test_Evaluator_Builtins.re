open Alcotest;
open Semantics;open Test_Evaluator_Prelude;
open IdTagged.FreshGrammar;
open Exp;

let tests = (
  "Evaluator.Builtins",
  [
    test_case("Builtin Function application", `Quick, () =>
      evaluation_test(
        "float_of_int(1)",
        float(1.0),
        ap(Forward, var("float_of_int"), int(1)),
      )
    ),
    test_case("Multi-arg builtin with cast", `Quick, () =>
      evaluation_test(
        {|string_compare(("Hello", "World"):(?, ?))|},
        int(-1),
        ap(
          Forward,
          builtin_fun("string_compare"),
          cast(
            tuple([
              cast(string("Hello"), Typ.string(), Typ.unknown(Internal)),
              cast(string("World"), Typ.string(), Typ.unknown(Internal)),
            ]),
            Typ.(prod([Typ.unknown(Internal), Typ.unknown(Internal)])),
            Typ.(prod([string(), string()])),
          ),
        ),
      )
    ),
    test_case("String_concat builtin", `Quick, () => {
      parse_and_evaluate_test(
        {|"hazel hello world"|},
        {|string_join(" ", ["hazel", "hello", "world"])|},
      )
    }),
    test_case("Multi arg builtin cast", `Quick, () =>
      evaluation_test(
        {|string_compare(("Hello", "World"):(?, ?))|},
        int(-1),
        ap(
          Forward,
          builtin_fun("string_compare"),
          cast(
            tuple([
              cast(string("Hello"), Typ.string(), Typ.unknown(Internal)),
              cast(string("World"), Typ.string(), Typ.unknown(Internal)),
            ]),
            Typ.(prod([Typ.unknown(Internal), Typ.unknown(Internal)])),
            Typ.(prod([string(), string()])),
          ),
        ),
      )
    ),
  ],
);
