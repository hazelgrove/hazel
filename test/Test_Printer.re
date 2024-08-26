open Alcotest;
open Haz3lcore;

let tests = (
  "Printer",
  [
    test_case("initial zipper", `Quick, () => {
      check(string, "Empty String", "", Printer.pretty_print(Zipper.init()))
    }),
  ],
);
