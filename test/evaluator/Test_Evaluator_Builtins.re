open Alcotest;
open Language;
open Test_Evaluator_Prelude;
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
    test_case("Multi-arg builtin with ascription", `Quick, () =>
      evaluation_test(
        {|string_compare(("Hello", "World"):(?, ?))|},
        int(-1),
        ap(
          Forward,
          builtin_fun("string_compare"),
          asc(
            tuple([string("Hello"), string("World")]),
            Typ.(prod([Typ.unknown(Ana), Typ.unknown(Ana)])),
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
    test_case("Multi arg builtin ascription", `Quick, () =>
      evaluation_test(
        {|string_compare(("Hello": ?, "World": ?):(?, ?))|},
        int(-1),
        ap(
          Forward,
          builtin_fun("string_compare"),
          asc(
            tuple([
              asc(string("Hello"), Typ.unknown(Ana)),
              asc(string("World"), Typ.unknown(Ana)),
            ]),
            Typ.(prod([Typ.unknown(Ana), Typ.unknown(Ana)])),
          ),
        ),
      )
    ),
    test_case("string_escaped", `Quick, () =>
      parse_and_evaluate_test({|"\\hello"|}, {|string_escaped("\hello")|})
    ),
    test_case("string_uppercase", `Quick, () =>
      parse_and_evaluate_test({|"HELLO"|}, {|string_uppercase("hello")|})
    ),
    test_case("string_lowercase", `Quick, () =>
      parse_and_evaluate_test({|"hello"|}, {|string_lowercase("HELLO")|})
    ),
    test_case("string_capitalize", `Quick, () =>
      parse_and_evaluate_test({|"Hello"|}, {|string_capitalize("hello")|})
    ),
    test_case("string_uncapitalize", `Quick, () =>
      parse_and_evaluate_test({|"hello"|}, {|string_uncapitalize("Hello")|})
    ),
    test_case("string_match true", `Quick, () =>
      evaluation_test(
        {|string_match(("hazel", "hazel"))|},
        bool(true),
        ap(
          Forward,
          builtin_fun("string_match"),
          tuple([string("hazel"), string("hazel")]),
        ),
      )
    ),
    test_case("string_match false", `Quick, () =>
      evaluation_test(
        {|string_match(("hazel", "world"))|},
        bool(false),
        ap(
          Forward,
          builtin_fun("string_match"),
          tuple([string("hazel"), string("world")]),
        ),
      )
    ),
    test_case("string_replace", `Quick, () =>
      evaluation_test(
        {|string_replace(("ha+zel", "haazelhzel", "world"))|},
        string("worldworld"),
        ap(
          Forward,
          builtin_fun("string_replace"),
          tuple([string("ha*zel"), string("hazelhzel"), string("world")]),
        ),
      )
    ),
    test_case("string_search found", `Quick, () =>
      evaluation_test(
        {|string_search(("h.+z", "hazel", 0))|},
        int(0),
        ap(
          Forward,
          builtin_fun("string_search"),
          tuple([string("h.+z"), string("hazel"), int(0)]),
        ),
      )
    ),
    test_case("string_search not found", `Quick, () =>
      evaluation_test(
        {|string_search(("foo", "hazel", 0))|},
        int(-1),
        ap(
          Forward,
          builtin_fun("string_search"),
          tuple([string("foo"), string("hazel"), int(0)]),
        ),
      )
    ),
  ],
);
