open Alcotest;
open Util;

let tests = (
  "ListUtil",
  [
    Alcotest.test_case(
      "rev_if with false",
      `Quick,
      () => {
        let xs = [1, 2, 3];
        check(list(int), "Same list", ListUtil.rev_if(false, xs), xs);
      },
    ),
    Alcotest.test_case(
      "rev_if with true",
      `Quick,
      () => {
        let xs = [1, 2, 3];
        check(
          list(int),
          "Reversed list",
          ListUtil.rev_if(true, xs),
          [3, 2, 1],
        );
      },
    ),
  ],
);
