open Alcotest;
open Util;

let tests = (
  "StringUtil",
  [
    test_case(
      "Unicode grapheme lengths",
      `Quick,
      () => {
        check(
          int,
          "Length of upside down question mark",
          1,
          StringUtil.length("¿"),
        );
        check(int, "Length of alpha character", 1, StringUtil.length("a"));
        check(
          int,
          "Length of multiple upside down question marks",
          3,
          StringUtil.length("¿¿¿"),
        );
      },
    ),
    test_case("regex matches upside down question mark", `Quick, () => {
      check(
        bool,
        "Match upside down question mark",
        true,
        StringUtil.match(StringUtil.regexp("^¿$"), "¿"),
      )
    }),
  ],
);
