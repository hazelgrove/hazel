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
        BuiltinsADT.Ord.lt,
        ap(
          Forward,
          builtin_fun("string_compare"),
          asc(
            tuple([string("Hello"), string("World")]),
            Typ.(prod([Typ.unknown(Internal), Typ.unknown(Internal)])),
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
        BuiltinsADT.Ord.lt,
        ap(
          Forward,
          builtin_fun("string_compare"),
          asc(
            tuple([
              asc(string("Hello"), Typ.unknown(Internal)),
              asc(string("World"), Typ.unknown(Internal)),
            ]),
            Typ.(prod([Typ.unknown(Internal), Typ.unknown(Internal)])),
          ),
        ),
      )
    ),
    test_case("string_escaped", `Quick, () =>
      parse_and_evaluate_test({|"\\\\hello"|}, {|string_escaped("\hello")|})
    ),
    test_case("string_escaped_tab", `Quick, () =>
      parse_and_evaluate_test({|"\\t"|}, {|string_escaped("\t")|})
    ),
    test_case("string_unescaped_backslash_n", `Quick, () =>
      parse_and_evaluate_test({|"\n"|}, {|string_unescaped("\\n")|})
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
    test_case("Raw string preserves literal backslash and n", `Quick, () =>
      parse_and_evaluate_test({|r"\n"|}, {|r"\n"|})
    ),
    test_case("Raw string preserves backslash t", `Quick, () =>
      parse_and_evaluate_test({|r"\t"|}, {|r"\t"|})
    ),
    test_case("string_escaped on raw string content", `Quick, () =>
      parse_and_evaluate_test({|"\\\\n"|}, {|string_escaped(r"\n")|})
    ),
    test_case("string_match with raw string regex pattern", `Quick, () =>
      evaluation_test(
        {|string_match((r"^hello.*", "hello world"))|},
        bool(true),
        ap(
          Forward,
          builtin_fun("string_match"),
          tuple([string({|^hello.*|}), string("hello world")]),
        ),
      )
    ),
    test_case(
      "string_search with raw string regex containing digit escape", `Quick, () =>
      evaluation_test(
        {|string_search((r"\d+", "abc123def", 0))|},
        int(3),
        ap(
          Forward,
          builtin_fun("string_search"),
          tuple([string({|\d+|}), string("abc123def"), int(0)]),
        ),
      )
    ),
    test_case("string_replace using raw string regex pattern", `Quick, () =>
      evaluation_test(
        {|string_replace((r"a+", "caaat", "o"))|},
        string("cot"),
        ap(
          Forward,
          builtin_fun("string_replace"),
          tuple([string({|a+|}), string("caaat"), string("o")]),
        ),
      )
    ),
    test_case("string_escaped multiple escapes (newline and tab)", `Quick, () =>
      parse_and_evaluate_test({|"\\n\\t"|}, {|string_escaped("\n\t")|})
    ),
    test_case("string_escaped single backslash", `Quick, () =>
      parse_and_evaluate_test({|"\\\\"|}, {|string_escaped("\\")|})
    ),
    test_case("string_unescaped tab character", `Quick, () =>
      parse_and_evaluate_test({|"\t"|}, {|string_unescaped("\\t")|})
    ),
    test_case("string_unescaped backslash", `Quick, () =>
      parse_and_evaluate_test({|"\\"|}, {|string_unescaped("\\\\")|})
    ),
    test_case(
      "string_match regex with standard string digit escape", `Quick, () =>
      evaluation_test(
        {|string_match(("\\d+", "99"))|},
        bool(true),
        ap(
          Forward,
          builtin_fun("string_match"),
          tuple([string("\\d+"), string("99")]),
        ),
      )
    ),
    test_case(
      "string_match regex matching literal backslash in standard string",
      `Quick,
      () =>
      evaluation_test(
        {|string_match(("\\\\", "\\"))|},
        bool(true),
        ap(
          Forward,
          builtin_fun("string_match"),
          tuple([string("\\\\"), string("\\")]),
        ),
      )
    ),
    test_case(
      "string_replace substituting escaped backslash in standard string",
      `Quick,
      () =>
      evaluation_test(
        {|string_replace(("\\\\", "a\\b", "/"))|},
        string("a/b"),
        ap(
          Forward,
          builtin_fun("string_replace"),
          tuple([string("\\\\"), string("a\\b"), string("/")]),
        ),
      )
    ),
  ],
);
