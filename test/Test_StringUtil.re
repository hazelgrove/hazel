open Alcotest;
open Util_web;

let tests = (
  "StringUtil",
  [
    test_case("empty string splits", `Quick, () => {
      check(list(string), "split", [], StringUtil.plain_split("", ""))
    }),
    test_case("split on empty string", `Quick, () => {
      check(list(string), "split", ["a"], StringUtil.plain_split("a", ""))
    }),
    test_case("split with no matches", `Quick, () => {
      check(list(string), "split", ["a"], StringUtil.plain_split("a", "b"))
    }),
    test_case("split with one match", `Quick, () => {
      check(
        list(string),
        "split",
        ["a", "c"],
        StringUtil.plain_split("abc", "b"),
      )
    }),
    test_case("split with multiple matches", `Quick, () => {
      check(
        list(string),
        "split",
        ["a", "c", "e"],
        StringUtil.plain_split("abcbe", "b"),
      )
    }),
    test_case("split with empty inbetweens", `Quick, () => {
      check(
        list(string),
        "split",
        ["a", "", "e"],
        StringUtil.plain_split("abbe", "b"),
      )
    }),
    test_case("regexp special character in separator", `Quick, () => {
      check(
        list(string),
        "split",
        ["a", "c"],
        StringUtil.plain_split("a.*c", ".*"),
      )
    }),
    test_case(
      "compress string",
      `Quick,
      () => {
        check(string, "empty string", "", StringUtil.compress(""));
        check(
          string,
          "ascii string",
          "abcdef.%2F%3F!%20",
          StringUtil.compress("abcdef./?! "),
        );
      },
    ),
    test_case(
      "decompress string",
      `Quick,
      () => {
        check(string, "empty string", "", StringUtil.decompress(""));
        check(
          string,
          "ascii string",
          "abcdef./?! ",
          StringUtil.decompress("abcdef.%2F%3F!%20"),
        );
      },
    ),
    test_case("sanitize", `Quick, () => {
      check(
        string,
        "The next pope",
        "GregoryCroisdaleJr",
        StringUtil.sanitize_filename("Gregory Croisdale Jr."),
      )
    }),
    test_case(
      "trim trailing whitespace should not trim leading whitespace", `Quick, () => {
      check(
        string,
        "trim trailing whitespace",
        " let a =\n\n  let b = 2 in\n\n  b",
        StringUtil.trim_trailing_whitespace(
          " let a = \n  \n  let b = 2 in  \n\n  b",
        ),
      )
    }),
    test_case(
      "trim trailing whitespace strips tabs and CR at line end", `Quick, () => {
      check(
        string,
        "tabs and cr trimmed at eol",
        "x\ny",
        StringUtil.trim_trailing_whitespace("x\t\r\ny \t"),
      )
    }),
    test_case("trim leading strips tabs after newline", `Quick, () => {
      check(
        string,
        "tabs after nl",
        "a\nb",
        StringUtil.trim_leading("a\n\t b"),
      )
    }),
    test_case("levenshtein: identical lists", `Quick, () => {
      check(
        int,
        "same",
        0,
        StringUtil.levenshtein_list_distance(["a", "b"], ["a", "b"]),
      )
    }),
    test_case("levenshtein: both empty", `Quick, () => {
      check(int, "empty", 0, StringUtil.levenshtein_list_distance([], []))
    }),
    test_case(
      "levenshtein: one empty",
      `Quick,
      () => {
        check(
          int,
          "a empty",
          2,
          StringUtil.levenshtein_list_distance([], ["x", "y"]),
        );
        check(
          int,
          "b empty",
          3,
          StringUtil.levenshtein_list_distance(["a", "b", "c"], []),
        );
      },
    ),
    test_case("levenshtein: single insertion", `Quick, () => {
      check(
        int,
        "insert",
        1,
        StringUtil.levenshtein_list_distance(["a", "b"], ["a", "x", "b"]),
      )
    }),
    test_case("levenshtein: single deletion", `Quick, () => {
      check(
        int,
        "delete",
        1,
        StringUtil.levenshtein_list_distance(["a", "x", "b"], ["a", "b"]),
      )
    }),
    test_case("levenshtein: single substitution", `Quick, () => {
      check(
        int,
        "subst",
        1,
        StringUtil.levenshtein_list_distance(["a", "b"], ["a", "c"]),
      )
    }),
    test_case("levenshtein: completely different", `Quick, () => {
      check(
        int,
        "diff",
        2,
        StringUtil.levenshtein_list_distance(["a", "b"], ["c", "d"]),
      )
    }),
  ],
);
