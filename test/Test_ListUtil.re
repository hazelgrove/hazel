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
    test_case(
      "combine_opt",
      `Quick,
      () => {
        check(
          option(list(pair(string, int))),
          "Same size lists",
          Some([("a", 1), ("b", 2), ("c", 3)]),
          ListUtil.combine_opt(["a", "b", "c"], [1, 2, 3]),
        );
        check(
          option(list(pair(string, int))),
          "Empty Lists",
          Some([]),
          ListUtil.combine_opt([], []),
        );
        check(
          option(list(pair(string, int))),
          "Inconsistent size lists",
          None,
          ListUtil.combine_opt(["a"], [1, 2]),
        );
      },
    ),
    test_case(
      "is_empty with empty list",
      `Quick,
      () => {
        let xs = [];
        check(bool, "Returns true", true, ListUtil.is_empty(xs));
      },
    ),
    test_case(
      "is_empty with non-empty list",
      `Quick,
      () => {
        let xs = [1, 2, 3];
        check(bool, "Returns false", false, ListUtil.is_empty(xs));
      },
    ),
    test_case(
      "flat_map with empty list",
      `Quick,
      () => {
        let xs = [];
        let f = x => [x, x];
        check(list(int), "Empty list", [], ListUtil.flat_map(f, xs));
      },
    ),
    test_case(
      "flat_map with non-empty list",
      `Quick,
      () => {
        let xs = [1, 2, 3];
        let f = x => [x, x];
        check(
          list(int),
          "Doubled list",
          [1, 1, 2, 2, 3, 3],
          ListUtil.flat_map(f, xs),
        );
      },
    ),
    test_case(
      "flat_map with non-empty list and empty result",
      `Quick,
      () => {
        let xs = [1, 2, 3];
        let f = _ => [];
        check(list(int), "Empty list", [], ListUtil.flat_map(f, xs));
      },
    ),
    test_case(
      "join with empty list",
      `Quick,
      () => {
        let xs = [];
        check(list(string), "Empty list", ListUtil.join(",", xs), []);
      },
    ),
    test_case(
      "join with single element list",
      `Quick,
      () => {
        let xs = ["a"];
        check(
          list(string),
          "Single element list",
          ListUtil.join(",", xs),
          ["a"],
        );
      },
    ),
    test_case(
      "join with multiple element list",
      `Quick,
      () => {
        let xs = ["a", "b", "c"];
        check(
          list(string),
          "Multiple element list",
          ListUtil.join(",", xs),
          ["a", ",", "b", ",", "c"],
        );
      },
    ),
    test_case(
      "hd_opt with empty list",
      `Quick,
      () => {
        let xs = [];
        check(option(int), "None", None, ListUtil.hd_opt(xs));
      },
    ),
    test_case(
      "hd_opt with non-empty list",
      `Quick,
      () => {
        let xs = [1, 2, 3];
        check(option(int), "Some", Some(1), ListUtil.hd_opt(xs));
      },
    ),
    test_case(
      "nth_opt with empty list",
      `Quick,
      () => {
        let xs = [];
        check(option(int), "None", None, ListUtil.nth_opt(0, xs));
      },
    ),
    test_case(
      "nth_opt with non-empty list",
      `Quick,
      () => {
        let xs = [1, 2, 3];
        check(option(int), "Some", Some(2), ListUtil.nth_opt(1, xs));
      },
    ),
    test_case(
      "nth_opt with out of bounds index",
      `Quick,
      () => {
        let xs = [1, 2, 3];
        check(option(int), "None", None, ListUtil.nth_opt(3, xs));
      },
    ),
    test_case(
      "split_n_opt with empty list",
      `Quick,
      () => {
        let xs = [];
        check(
          option(pair(list(int), list(int))),
          "Empty list",
          Some(([], [])),
          ListUtil.split_n_opt(0, xs),
        );
      },
    ),
    test_case(
      "split_n_opt with non-empty list",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          option(pair(list(int), list(int))),
          "Split list",
          Some(([1, 2, 3], [4, 5])),
          ListUtil.split_n_opt(3, xs),
        );
      },
    ),
    test_case(
      "split_n_opt with out of bounds index",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          option(pair(list(int), list(int))),
          "None",
          None,
          ListUtil.split_n_opt(6, xs),
        );
      },
    ),
    test_case(
      "split_n_opt with zero index",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          option(pair(list(int), list(int))),
          "Empty first part",
          Some(([], xs)),
          ListUtil.split_n_opt(0, xs),
        );
      },
    ),
    test_case(
      "split_n with empty list",
      `Quick,
      () => {
        let xs = [];
        check(
          pair(list(int), list(int)),
          "Empty list",
          ([], []),
          ListUtil.split_n(0, xs),
        );
      },
    ),
    test_case(
      "split_n with non-empty list",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          pair(list(int), list(int)),
          "Split list",
          ([1, 2, 3], [4, 5]),
          ListUtil.split_n(3, xs),
        );
      },
    ),
    test_case(
      "split_n with out of bounds index",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check_raises(
          "raises invalid argument",
          Invalid_argument("ListUtil.split_n: 6"),
          () => {
            let _ = ListUtil.split_n(6, xs);
            ();
          },
        );
      },
    ),
    test_case(
      "split_n with zero index",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          pair(list(int), list(int)),
          "Empty first part",
          ([], xs),
          ListUtil.split_n(0, xs),
        );
      },
    ),
    test_case(
      "split_sublist_opt with empty list",
      `Quick,
      () => {
        let xs = [];
        check(
          option(triple(list(int), list(int), list(int))),
          "Empty list",
          Some(([], [], [])),
          ListUtil.split_sublist_opt(0, 0, xs),
        );
      },
    ),
    test_case(
      "split_sublist_opt with non-empty list",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          option(triple(list(int), list(int), list(int))),
          "Split list",
          Some(([1, 2], [3, 4], [5])),
          ListUtil.split_sublist_opt(2, 4, xs),
        );
      },
    ),
    test_case(
      "split_sublist_opt with out of bounds index",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          option(triple(list(int), list(int), list(int))),
          "None",
          None,
          ListUtil.split_sublist_opt(6, 7, xs),
        );
      },
    ),
    test_case(
      "split_sublist_opt with zero index",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          option(triple(list(int), list(int), list(int))),
          "Empty first part",
          Some(([], [], xs)),
          ListUtil.split_sublist_opt(0, 0, xs),
        );
      },
    ),
    test_case(
      "split_sublist with empty list",
      `Quick,
      () => {
        let xs = [];
        check(
          triple(list(int), list(int), list(int)),
          "Empty list",
          ([], [], []),
          ListUtil.split_sublist(0, 0, xs),
        );
      },
    ),
    test_case(
      "split_sublist with non-empty list",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          triple(list(int), list(int), list(int)),
          "Split list",
          ([1, 2], [3, 4], [5]),
          ListUtil.split_sublist(2, 4, xs),
        );
      },
    ),
    test_case(
      "split_sublist with out of bounds index",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check_raises(
          "raises invalid argument",
          Invalid_argument("ListUtil.split_sublist: 6, 7"),
          () => {
            let _ = ListUtil.split_sublist(6, 7, xs);
            ();
          },
        );
      },
    ),
    test_case(
      "split_sublist with zero index",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          triple(list(int), list(int), list(int)),
          "Empty first part",
          ([], [], xs),
          ListUtil.split_sublist(0, 0, xs),
        );
      },
    ),
    test_case(
      "sublist with empty list",
      `Quick,
      () => {
        let xs = [];
        check(list(int), "Empty list", [], ListUtil.sublist((0, 0), xs));
      },
    ),
    test_case(
      "sublist with non-empty list",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          list(int),
          "Sublist",
          [2, 3, 4],
          ListUtil.sublist((1, 4), xs),
        );
      },
    ),
    test_case(
      "sublist with out of bounds index",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check_raises(
          "raises invalid argument",
          Invalid_argument("ListUtil.split_sublist: 6, 7"),
          () => {
            let _ = ListUtil.sublist((6, 7), xs);
            ();
          },
        );
      },
    ),
  ],
);
