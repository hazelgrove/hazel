open Alcotest;
open Util;

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
  ],
);
