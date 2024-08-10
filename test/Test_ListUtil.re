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
        check(list(int), "Same list", xs, ListUtil.rev_if(false, xs));
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
          [3, 2, 1],
          ListUtil.rev_if(true, xs),
        );
      },
    ),
    Alcotest.test_case(
      "dedup",
      `Quick,
      () => {
        let xs = [1, 2, 3, 2];
        check(list(int), "Unique list", [1, 3, 2], ListUtil.dedup(xs)); // TODO: Interesting the order here is messed up because of fold_right
      },
    ),
    Alcotest.test_case(
      "dedup_f",
      `Quick,
      () => {
        let xs = [1, 2, 3, 2];
        check(
          list(int),
          "Unique list",
          [1, 3, 2],
          ListUtil.dedup_f((==), xs),
        ); // TODO: Interesting the order here is messed up because of fold_right
      },
    ),
    Alcotest.test_case(
      "are_duplicates has duplicates",
      `Quick,
      () => {
        let xs = [1, 2, 3, 2];
        check(bool, "Returns false", false, ListUtil.are_duplicates(xs)); // TODO: Interesting the order here is messed up because of fold_right
      },
    ),
    Alcotest.test_case(
      "are_duplicates has no duplicates",
      `Quick,
      () => {
        let xs = [1, 2, 3];
        check(bool, "Returns true", true, ListUtil.are_duplicates(xs)); // TODO: Interesting the order here is messed up because of fold_right
      },
    ),
  ],
);
