open Alcotest;
open Util;

let tests = (
  "ListUtil",
  [
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
    test_case("slice with negative i", `Quick, () => {
      check(
        list(int),
        "Starts at index 0",
        [1, 2],
        ListUtil.slice(-1, 2, [1, 2, 3]),
      )
    }),
    test_case("slice with negative k", `Quick, () => {
      check(list(int), "Nothing", [], ListUtil.slice(1, -1, [1, 2, 3]))
    }),
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
