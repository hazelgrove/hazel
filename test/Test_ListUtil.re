open Alcotest;
open Util;

let tests = (
  "ListUtil",
  [
    test_case(
      "rev_if with false",
      `Quick,
      () => {
        let xs = [1, 2, 3];
        check(list(int), "Same list", xs, ListUtil.rev_if(false, xs));
      },
    ),
    test_case(
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
    test_case(
      "dedup",
      `Quick,
      () => {
        let xs = [1, 2, 3, 2];
        check(list(int), "Unique list", [1, 3, 2], ListUtil.dedup(xs)); // TODO: Interesting the order here is messed up because of fold_right
      },
    ),
    test_case(
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
    test_case(
      "are_duplicates has duplicates",
      `Quick,
      () => {
        let xs = [1, 2, 3, 2];
        check(bool, "Returns false", false, ListUtil.are_duplicates(xs)); // TODO: Interesting the order here is messed up because of fold_right
      },
    ),
    test_case(
      "are_duplicates has no duplicates",
      `Quick,
      () => {
        let xs = [1, 2, 3];
        check(bool, "Returns true", true, ListUtil.are_duplicates(xs)); // TODO: Interesting the order here is messed up because of fold_right
      },
    ),
    test_case(
      "group_by with constant function preserves list",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          list(pair(unit, list(int))),
          "singleton group",
          [((), List.rev(xs))],
          ListUtil.group_by(__ => (), xs),
        );
      },
    ),
    test_case(
      "group_by groups into evens/odds",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          list(pair(int, list(int))),
          "odds and evens",
          [(1, [5, 3, 1]), (0, [4, 2])],
          ListUtil.group_by(x => x mod 2, xs),
        );
      },
    ),
    test_case("range generates sequential integers [1,6)", `Quick, () => {
      check(list(int), "1-5", [1, 2, 3, 4, 5], ListUtil.range(~lo=1, 6))
    }),
    test_case("range defaults lower bound to 0", `Quick, () => {
      check(list(int), "0-5", [0, 1, 2, 3, 4, 5], ListUtil.range(6))
    }),
    test_case("range lo = hi is empty", `Quick, () => {
      check(list(int), "empty list", [], ListUtil.range(~lo=1, 1))
    }),
    test_case("Invalid range raises error", `Quick, () => {
      check_raises(
        "Invalid range",
        Invalid_argument("ListUtil.range"),
        () => {
          let _ = ListUtil.range(~lo=2, 1);
          ();
        },
      )
    }),
    test_case(
      "mk_frame creates a frame from the beginning",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          pair(list(int), list(int)),
          "frame",
          ([], xs),
          ListUtil.mk_frame(0, xs),
        );
      },
    ),
    test_case(
      "mk_frame creates a frame from the end",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          pair(list(int), list(int)),
          "frame",
          (List.rev(xs), []),
          ListUtil.mk_frame(5, xs),
        );
      },
    ),
    test_case(
      "mk_frame raises when making a frame past the end",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check_raises(
          "raises invalid argument",
          Invalid_argument("ListUtil.mk_frame"),
          () => {
            let _ = ListUtil.mk_frame(6, xs);
            ();
          },
        );
      },
    ),
    test_case(
      "mk_frame raises when making a frame before the beginning",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check_raises(
          "raises invalid argument",
          Invalid_argument("ListUtil.mk_frame"),
          () => {
            let _ = ListUtil.mk_frame(-1, xs);
            ();
          },
        );
      },
    ),
    test_case(
      "mk_frame makes a frame splitting the list",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          pair(list(int), list(int)),
          "frame",
          (List.rev([1, 2, 3]), [4, 5]),
          ListUtil.mk_frame(3, xs),
        );
      },
    ),
    test_case(
      "mk_frame makes a frame splitting the list",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          pair(list(int), list(int)),
          "frame",
          (List.rev([1, 2, 3]), [4, 5]),
          ListUtil.mk_frame(3, xs),
        );
      },
    ),
    test_case(
      "split with no found element returns the original list",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          triple(list(int), option(int), list(int)),
          "split",
          (xs, None, []),
          ListUtil.split(xs, __ => false),
        );
      },
    ),
    test_case(
      "split with first found returns the head and tail",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          triple(list(int), option(int), list(int)),
          "split",
          ([], Some(1), [2, 3, 4, 5]),
          ListUtil.split(xs, __ => true),
        );
      },
    ),
    test_case(
      "splits on the middle element",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          triple(list(int), option(int), list(int)),
          "split",
          ([1, 2], Some(3), [4, 5]),
          ListUtil.split(xs, (==)(3)),
        );
      },
    ),
  ],
);
