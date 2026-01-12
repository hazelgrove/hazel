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
        check(list(int), "Unique list", [1, 2, 3], ListUtil.dedup(xs));
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
          [1, 2, 3],
          ListUtil.dedup_f((==), xs),
        );
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
    test_case(
      "group_by groups into mod 10 reverses group ordering",
      `Quick,
      () => {
        let xs = [71, 69, 60, 79, 70, 72, 51, 79, 46, 6];
        check(
          list(pair(int, list(int))),
          "odds and evens",
          [
            (0, [6]),
            (4, [46]),
            (7, [79, 72, 70, 79, 71]),
            (5, [51]),
            (6, [60, 69]),
          ],
          ListUtil.group_by(x => x / 10, xs),
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
    test_case(
      "find with rest",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          option(pair(string, list(int))),
          "Found",
          Some(("found", [1, 2, 4, 5])),
          ListUtil.find_with_rest(i => i > 2 ? Some("found") : None, xs),
        );
        check(
          option(pair(string, list(int))),
          "Not found",
          None,
          ListUtil.find_with_rest(i => i > 5 ? Some("found") : None, xs),
        );
      },
    ),
    test_case("last_opt with empty list", `Quick, () => {
      check(option(int), "None for empty list", None, ListUtil.last_opt([]))
    }),
    test_case("last_opt with single element", `Quick, () => {
      check(
        option(int),
        "Some element for single element list",
        Some(42),
        ListUtil.last_opt([42]),
      )
    }),
    test_case(
      "last_opt with multiple elements",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          option(int),
          "Some last element",
          Some(5),
          ListUtil.last_opt(xs),
        );
      },
    ),
    test_case("last with single element", `Quick, () => {
      check(
        int,
        "Last element of single element list",
        42,
        ListUtil.last([42]),
      )
    }),
    test_case(
      "last with multiple elements",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(int, "Last element", 5, ListUtil.last(xs));
      },
    ),
    test_case("last with empty list raises exception", `Quick, () => {
      check_raises(
        "Invalid_argument exception", Invalid_argument("ListUtil.last"), () =>
        ListUtil.last([])
      )
    }),
    test_case(
      "map_alt with single x element",
      `Quick,
      () => {
        let xs = [1];
        let ys = [];
        let result = ListUtil.map_alt(x => x * 2, y => y + 10, xs, ys);
        check(list(int), "Single element mapped", [2], result);
      },
    ),
    test_case(
      "map_alt alternates functions correctly",
      `Quick,
      () => {
        let xs = [1, 3, 5];
        let ys = [2, 4];
        let result = ListUtil.map_alt(x => x * 10, y => y + 100, xs, ys);
        check(
          list(int),
          "Alternated mapping",
          [10, 102, 30, 104, 50],
          result,
        );
      },
    ),
    test_case("map_alt with equal length lists raises exception", `Quick, () => {
      check_raises(
        "Invalid_argument exception", Invalid_argument("ListUtil.map_alt"), () => {
        ignore(ListUtil.map_alt(x => x, y => y, [1, 2], [3, 4]))
      })
    }),
    test_case("map_alt with xs shorter than ys raises exception", `Quick, () => {
      check_raises(
        "Invalid_argument exception", Invalid_argument("ListUtil.map_alt"), () => {
        ignore(ListUtil.map_alt(x => x, y => y, [1], [2, 3]))
      })
    }),
    test_case(
      "interleave with single x element",
      `Quick,
      () => {
        let xs = [1];
        let ys = [];
        let result = ListUtil.interleave(xs, ys);
        check(list(int), "Single element interleaved", [1], result);
      },
    ),
    test_case(
      "interleave basic case",
      `Quick,
      () => {
        let xs = [1, 3, 5];
        let ys = [2, 4];
        let result = ListUtil.interleave(xs, ys);
        check(list(int), "Basic interleaving", [1, 2, 3, 4, 5], result);
      },
    ),
    test_case("interleave inherits map_alt length constraint", `Quick, () => {
      check_raises(
        "Invalid_argument exception", Invalid_argument("ListUtil.map_alt"), () => {
        ignore(ListUtil.interleave([1, 2], [3, 4]))
      })
    }),
    test_case("truncate with empty list", `Quick, () => {
      check(list(int), "Empty list", [], ListUtil.truncate(5, []))
    }),
    test_case(
      "truncate with n=0",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(list(int), "Zero elements", [], ListUtil.truncate(0, xs));
      },
    ),
    test_case(
      "truncate less than list length",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          list(int),
          "First 3 elements",
          [1, 2, 3],
          ListUtil.truncate(3, xs),
        );
      },
    ),
    test_case(
      "truncate equal to list length",
      `Quick,
      () => {
        let xs = [1, 2, 3];
        check(
          list(int),
          "All elements",
          [1, 2, 3],
          ListUtil.truncate(3, xs),
        );
      },
    ),
    test_case(
      "truncate greater than list length",
      `Quick,
      () => {
        let xs = [1, 2, 3];
        check(
          list(int),
          "All elements when n > length",
          [1, 2, 3],
          ListUtil.truncate(10, xs),
        );
      },
    ),
    test_case("remove_first_n with empty list", `Quick, () => {
      check(list(int), "Empty list", [], ListUtil.remove_first_n(3, []))
    }),
    test_case(
      "remove_first_n with n=0",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          list(int),
          "Remove 0 elements",
          xs,
          ListUtil.remove_first_n(0, xs),
        );
      },
    ),
    test_case(
      "remove_first_n less than list length",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          list(int),
          "Remove first 2 elements",
          [3, 4, 5],
          ListUtil.remove_first_n(2, xs),
        );
      },
    ),
    test_case(
      "remove_first_n equal to list length",
      `Quick,
      () => {
        let xs = [1, 2, 3];
        check(
          list(int),
          "Remove all elements",
          [],
          ListUtil.remove_first_n(3, xs),
        );
      },
    ),
    test_case(
      "remove_first_n greater than list length",
      `Quick,
      () => {
        let xs = [1, 2, 3];
        check(
          list(int),
          "Remove more than length",
          [],
          ListUtil.remove_first_n(10, xs),
        );
      },
    ),
    test_case("slice with empty list", `Quick, () => {
      check(list(int), "Empty list", [], ListUtil.slice(2, 3, []))
    }),
    test_case(
      "slice from beginning",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          list(int),
          "Slice from start",
          [1, 2, 3],
          ListUtil.slice(0, 3, xs),
        );
      },
    ),
    test_case(
      "slice from middle",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          list(int),
          "Slice from middle",
          [2, 3, 4],
          ListUtil.slice(1, 3, xs),
        );
      },
    ),
    test_case(
      "slice with k=0",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(list(int), "Zero length slice", [], ListUtil.slice(2, 0, xs));
      },
    ),
    test_case(
      "slice beyond list bounds",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          list(int),
          "Slice beyond bounds",
          [],
          ListUtil.slice(10, 3, xs),
        );
      },
    ),
    test_case(
      "slice with large k",
      `Quick,
      () => {
        let xs = [1, 2, 3, 4, 5];
        check(
          list(int),
          "Slice to end",
          [3, 4, 5],
          ListUtil.slice(2, 10, xs),
        );
      },
    ),
    test_case(
      "group_consecutive groups adjacent equal elements",
      `Quick,
      () => {
        let xs = [1, 1, 2, 2, 2, 3, 1, 1];
        check(
          list(list(int)),
          "Groups consecutive runs",
          [[1, 1], [2, 2, 2], [3], [1, 1]] |> List.rev,
          ListUtil.group_consecutive((==), xs),
        );
      },
    ),
    test_case(
      "group_consecutive with custom predicate",
      `Quick,
      () => {
        /* Group consecutive numbers that are both even or both odd */
        let same_parity = (a: int, b: int): bool => a mod 2 == b mod 2;
        let xs = [2, 4, 3, 5, 7, 8, 10];
        check(
          list(list(int)),
          "Groups by parity runs",
          [[2, 4], [3, 5, 7], [8, 10]] |> List.rev,
          ListUtil.group_consecutive(same_parity, xs),
        );
      },
    ),
  ],
);
