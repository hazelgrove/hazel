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
  ],
);
