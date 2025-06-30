open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open IdTagged.FreshGrammar;
open Exp;

// Instead of using these explicitly we could call elaboration in the tests/use the parse_and_evaluate_test
let some =
  Exp.constructor(
    "Some",
    Some(
      Some(
        Typ.(arrow(unknown(SynSwitch), Builtins.TypeAliases.option_type)),
      ),
    ),
  );
let none =
  Exp.constructor("None", Some(Some(Builtins.TypeAliases.option_type)));

let tests = (
  "Evaluator.ListBuiltins",
  [
    test_case("length of empty list", `Quick, () =>
      evaluation_test(
        "length([])",
        int(0),
        ap(Forward, builtin_fun("length"), list_lit([])),
      )
    ),
    test_case("length of multi-element list", `Quick, () =>
      evaluation_test(
        "length([1, 2, 3])",
        int(3),
        ap(
          Forward,
          builtin_fun("length"),
          list_lit([int(1), int(2), int(3)]),
        ),
      )
    ),
    test_case("map with identity function", `Quick, () =>
      evaluation_test(
        "map([1, 2, 3], fun x -> x)",
        list_lit([int(1), int(2), int(3)]),
        ap(
          Forward,
          builtin_fun("map"),
          tuple([
            list_lit([int(1), int(2), int(3)]),
            fn(Pat.var("x"), var("x"), None, None),
          ]),
        ),
      )
    ),
    test_case("map with increment function", `Quick, () =>
      evaluation_test(
        "map([1, 2, 3], fun x -> x + 1)",
        list_lit([int(2), int(3), int(4)]),
        ap(
          Forward,
          builtin_fun("map"),
          tuple([
            list_lit([int(1), int(2), int(3)]),
            fn(
              Pat.var("x"),
              bin_op(Int(Plus), var("x"), int(1)),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("filter even numbers", `Quick, () =>
      evaluation_test(
        "filter([1, 2, 3, 4], fun x -> int_mod(x, 2) == 0)",
        list_lit([int(2), int(4)]),
        ap(
          Forward,
          builtin_fun("filter"),
          tuple([
            list_lit([int(1), int(2), int(3), int(4)]),
            fn(
              Pat.var("x"),
              bin_op(
                Int(Equals),
                ap(
                  Forward,
                  builtin_fun("int_mod"),
                  tuple([var("x"), int(2)]),
                ),
                int(0),
              ),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("fold_left sum", `Quick, () =>
      evaluation_test(
        "fold_left([1, 2, 3], fun (acc, x) -> acc + x, 0)",
        int(6),
        ap(
          Forward,
          builtin_fun("fold_left"),
          tuple([
            list_lit([int(1), int(2), int(3)]),
            fn(
              Pat.tuple([Pat.var("acc"), Pat.var("x")]),
              bin_op(Int(Plus), var("acc"), var("x")),
              None,
              None,
            ),
            int(0),
          ]),
        ),
      )
    ),
    test_case("flat_map duplicate elements", `Quick, () =>
      evaluation_test(
        "flat_map([1, 2], fun x -> [x, x])",
        list_lit([int(2), int(2), int(1), int(1)]),
        ap(
          Forward,
          builtin_fun("flat_map"),
          tuple([
            list_lit([int(1), int(2)]),
            fn(Pat.var("x"), list_lit([var("x"), var("x")]), None, None),
          ]),
        ),
      )
    ),
    test_case("zip two lists", `Quick, () =>
      evaluation_test(
        "zip([1, 2], [3, 4])",
        list_lit([tuple([int(1), int(3)]), tuple([int(2), int(4)])]),
        ap(
          Forward,
          builtin_fun("zip"),
          tuple([
            list_lit([int(1), int(2)]),
            list_lit([int(3), int(4)]),
          ]),
        ),
      )
    ),
    test_case("unzip list of pairs", `Quick, () =>
      evaluation_test(
        "unzip([(1, 3), (2, 4)])",
        tuple([list_lit([int(1), int(2)]), list_lit([int(3), int(4)])]),
        ap(
          Forward,
          builtin_fun("unzip"),
          list_lit([tuple([int(1), int(3)]), tuple([int(2), int(4)])]),
        ),
      )
    ),
    test_case("reverse list", `Quick, () =>
      evaluation_test(
        "reverse([1, 2, 3])",
        list_lit([int(3), int(2), int(1)]),
        ap(
          Forward,
          builtin_fun("reverse"),
          list_lit([int(1), int(2), int(3)]),
        ),
      )
    ),
    test_case("take first two elements", `Quick, () =>
      evaluation_test(
        "take([1, 2, 3], 2)",
        list_lit([int(1), int(2)]),
        ap(
          Forward,
          builtin_fun("take"),
          tuple([list_lit([int(1), int(2), int(3)]), int(2)]),
        ),
      )
    ),
    test_case("drop first two elements", `Quick, () =>
      evaluation_test(
        "drop([1, 2, 3], 2)",
        list_lit([int(3)]),
        ap(
          Forward,
          builtin_fun("drop"),
          tuple([list_lit([int(1), int(2), int(3)]), int(2)]),
        ),
      )
    ),
    test_case("range from 1 to 3", `Quick, () =>
      evaluation_test(
        "range(1, 3)",
        list_lit([int(1), int(2), int(3)]),
        ap(Forward, builtin_fun("range"), tuple([int(1), int(3)])),
      )
    ),
    test_case("enumerate list", `Quick, () =>
      evaluation_test(
        "enumerate([10, 20])",
        list_lit([tuple([int(0), int(10)]), tuple([int(1), int(20)])]),
        ap(
          Forward,
          builtin_fun("enumerate"),
          list_lit([int(10), int(20)]),
        ),
      )
    ),
    test_case("any with true predicate", `Quick, () =>
      evaluation_test(
        "any([1, 2, 3], fun x -> x > 2)",
        bool(true),
        ap(
          Forward,
          builtin_fun("any"),
          tuple([
            list_lit([int(1), int(2), int(3)]),
            fn(
              Pat.var("x"),
              bin_op(Int(GreaterThan), var("x"), int(2)),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("all with false predicate", `Quick, () =>
      evaluation_test(
        "all([1, 2, 3], fun x -> x > 5)",
        bool(false),
        ap(
          Forward,
          builtin_fun("all"),
          tuple([
            list_lit([int(1), int(2), int(3)]),
            fn(
              Pat.var("x"),
              bin_op(Int(GreaterThan), var("x"), int(5)),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("intersperse with separator", `Quick, () =>
      evaluation_test(
        "intersperse([1, 2], 0)",
        list_lit([int(1), int(0), int(2)]),
        ap(
          Forward,
          builtin_fun("intersperse"),
          tuple([list_lit([int(1), int(2)]), int(0)]),
        ),
      )
    ),
    test_case("cons element to list", `Quick, () =>
      evaluation_test(
        "cons(1, [2, 3])",
        list_lit([int(1), int(2), int(3)]),
        ap(
          Forward,
          builtin_fun("cons"),
          tuple([int(1), list_lit([int(2), int(3)])]),
        ),
      )
    ),
    test_case("hd of non-empty list", `Quick, () =>
      evaluation_test(
        "hd([1, 2, 3])",
        int(1),
        ap(
          Forward,
          builtin_fun("hd"),
          list_lit([int(1), int(2), int(3)]),
        ),
      )
    ),
    test_case("tl of non-empty list", `Quick, () =>
      evaluation_test(
        "tl([1, 2, 3])",
        list_lit([int(2), int(3)]),
        ap(
          Forward,
          builtin_fun("tl"),
          list_lit([int(1), int(2), int(3)]),
        ),
      )
    ),
    test_case("is_empty of empty list", `Quick, () =>
      evaluation_test(
        "is_empty([])",
        bool(true),
        ap(Forward, builtin_fun("is_empty"), list_lit([])),
      )
    ),
    test_case("nth element at index 1", `Quick, () =>
      evaluation_test(
        "nth([1, 2, 3], 1)",
        int(2),
        ap(
          Forward,
          builtin_fun("nth"),
          tuple([list_lit([int(1), int(2), int(3)]), int(1)]),
        ),
      )
    ),
    test_case("fold_right subtract", `Quick, () =>
      evaluation_test(
        "fold_right([1, 2, 3], fun (x, acc) -> x - acc, 0)",
        int(2),
        ap(
          Forward,
          builtin_fun("fold_right"),
          tuple([
            list_lit([int(1), int(2), int(3)]),
            fn(
              Pat.tuple([Pat.var("x"), Pat.var("acc")]),
              bin_op(Int(Minus), var("x"), var("acc")),
              None,
              None,
            ),
            int(0),
          ]),
        ),
      )
    ),
    test_case("append two lists", `Quick, () =>
      evaluation_test(
        "append([1, 2], [3, 4])",
        list_lit([int(1), int(2), int(3), int(4)]),
        ap(
          Forward,
          builtin_fun("append"),
          tuple([
            list_lit([int(1), int(2)]),
            list_lit([int(3), int(4)]),
          ]),
        ),
      )
    ),
    test_case("concat list of lists", `Quick, () =>
      evaluation_test(
        "concat([[1, 2], [3], [4, 5]])",
        list_lit([int(1), int(2), int(3), int(4), int(5)]),
        ap(
          Forward,
          builtin_fun("concat"),
          list_lit([
            list_lit([int(1), int(2)]),
            list_lit([int(3)]),
            list_lit([int(4), int(5)]),
          ]),
        ),
      )
    ),
    test_case("mapi with index", `Quick, () =>
      evaluation_test(
        "mapi([10, 20], fun (i, x) -> i + x)",
        list_lit([int(10), int(21)]),
        ap(
          Forward,
          builtin_fun("mapi"),
          tuple([
            list_lit([int(10), int(20)]),
            fn(
              Pat.tuple([Pat.var("i"), Pat.var("x")]),
              bin_op(Int(Plus), var("i"), var("x")),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("filteri even indices", `Quick, () =>
      evaluation_test(
        "filteri([1, 2, 3, 4], fun (i, x) -> int_mod(i, 2) == 0)",
        list_lit([int(1), int(3)]),
        ap(
          Forward,
          builtin_fun("filteri"),
          tuple([
            list_lit([int(1), int(2), int(3), int(4)]),
            fn(
              Pat.tuple([Pat.var("i"), Pat.var("x")]),
              bin_op(
                Int(Equals),
                ap(
                  Forward,
                  builtin_fun("int_mod"),
                  tuple([var("i"), int(2)]),
                ),
                int(0),
              ),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("mem element in list", `Quick, () =>
      evaluation_test(
        "mem([1, 2, 3], 2)",
        bool(true),
        ap(
          Forward,
          builtin_fun("mem"),
          tuple([list_lit([int(1), int(2), int(3)]), int(2)]),
        ),
      )
    ),
    test_case("partition even and odd", `Quick, () =>
      evaluation_test(
        "partition([1, 2, 3, 4], fun x -> int_mod(x, 2) == 0)",
        tuple([list_lit([int(2), int(4)]), list_lit([int(1), int(3)])]),
        ap(
          Forward,
          builtin_fun("partition"),
          tuple([
            list_lit([int(1), int(2), int(3), int(4)]),
            fn(
              Pat.var("x"),
              bin_op(
                Int(Equals),
                ap(
                  Forward,
                  builtin_fun("int_mod"),
                  tuple([var("x"), int(2)]),
                ),
                int(0),
              ),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("rev_append reversed list", `Quick, () =>
      evaluation_test(
        "rev_append([1, 2], [3, 4])",
        list_lit([int(2), int(1), int(3), int(4)]),
        ap(
          Forward,
          builtin_fun("rev_append"),
          tuple([
            list_lit([int(1), int(2)]),
            list_lit([int(3), int(4)]),
          ]),
        ),
      )
    ),
    test_case("fold_left2 sum pairs", `Quick, () =>
      evaluation_test(
        "fold_left2([1, 2], [3, 4], fun (acc, x, y) -> acc + x + y, 0)",
        int(10),
        ap(
          Forward,
          builtin_fun("fold_left2"),
          tuple([
            list_lit([int(1), int(2)]),
            list_lit([int(3), int(4)]),
            fn(
              Pat.tuple([Pat.var("acc"), Pat.var("x"), Pat.var("y")]),
              bin_op(
                Int(Plus),
                bin_op(Int(Plus), var("acc"), var("x")),
                var("y"),
              ),
              None,
              None,
            ),
            int(0),
          ]),
        ),
      )
    ),
    test_case("fold_right2 subtract pairs", `Quick, () =>
      evaluation_test(
        "fold_right2([1, 2], [3, 4], fun (x, y, acc) -> x + y - acc, 0)",
        int(-2),
        ap(
          Forward,
          builtin_fun("fold_right2"),
          tuple([
            list_lit([int(1), int(2)]),
            list_lit([int(3), int(4)]),
            fn(
              Pat.tuple([Pat.var("x"), Pat.var("y"), Pat.var("acc")]),
              bin_op(
                Int(Minus),
                bin_op(Int(Plus), var("x"), var("y")),
                var("acc"),
              ),
              None,
              None,
            ),
            int(0),
          ]),
        ),
      )
    ),
    test_case("map2 add pairs", `Quick, () =>
      evaluation_test(
        "map2([1, 2], [3, 4], fun (x, y) -> x + y)",
        list_lit([int(4), int(6)]),
        ap(
          Forward,
          builtin_fun("map2"),
          tuple([
            list_lit([int(1), int(2)]),
            list_lit([int(3), int(4)]),
            fn(
              Pat.tuple([Pat.var("x"), Pat.var("y")]),
              bin_op(Int(Plus), var("x"), var("y")),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("all2 compare pairs", `Quick, () =>
      evaluation_test(
        "all2([1, 2], [3, 4], fun (x, y) -> x < y)",
        bool(true),
        ap(
          Forward,
          builtin_fun("all2"),
          tuple([
            list_lit([int(1), int(2)]),
            list_lit([int(3), int(4)]),
            fn(
              Pat.tuple([Pat.var("x"), Pat.var("y")]),
              bin_op(Int(LessThan), var("x"), var("y")),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("any2 find equal pairs", `Quick, () =>
      evaluation_test(
        "any2([1, 2], [3, 2], fun (x, y) -> x == y)",
        bool(true),
        ap(
          Forward,
          builtin_fun("any2"),
          tuple([
            list_lit([int(1), int(2)]),
            list_lit([int(3), int(2)]),
            fn(
              Pat.tuple([Pat.var("x"), Pat.var("y")]),
              bin_op(Int(Equals), var("x"), var("y")),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("find first even number", `Quick, () =>
      evaluation_test(
        "find([1, 2, 3, 4], fun x -> int_mod(x, 2) == 0)",
        int(2),
        ap(
          Forward,
          builtin_fun("find"),
          tuple([
            list_lit([int(1), int(2), int(3), int(4)]),
            fn(
              Pat.var("x"),
              bin_op(
                Int(Equals),
                ap(
                  Forward,
                  builtin_fun("int_mod"),
                  tuple([var("x"), int(2)]),
                ),
                int(0),
              ),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("take_while positive numbers", `Quick, () =>
      evaluation_test(
        "take_while([1, 2, -3, 4], fun x -> x > 0)",
        list_lit([int(1), int(2)]),
        ap(
          Forward,
          builtin_fun("take_while"),
          tuple([
            list_lit([int(1), int(2), int(-3), int(4)]),
            fn(
              Pat.var("x"),
              bin_op(Int(GreaterThan), var("x"), int(0)),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("drop_while positive numbers", `Quick, () =>
      evaluation_test(
        "drop_while([1, 2, -3, 4], fun x -> x > 0)",
        list_lit([int(-3), int(4)]),
        ap(
          Forward,
          builtin_fun("drop_while"),
          tuple([
            list_lit([int(1), int(2), int(-3), int(4)]),
            fn(
              Pat.var("x"),
              bin_op(Int(GreaterThan), var("x"), int(0)),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("sort empty list", `Quick, () =>
      evaluation_test(
        "sort([], fun (x, y) -> x - y)",
        list_lit([]),
        ap(
          Forward,
          builtin_fun("sort"),
          tuple([
            list_lit([]),
            fn(
              Pat.tuple([Pat.var("x"), Pat.var("y")]),
              bin_op(Int(Minus), var("x"), var("y")),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("sort singleton list", `Quick, () =>
      evaluation_test(
        "sort([1], fun (x, y) -> x - y)",
        list_lit([int(1)]),
        ap(
          Forward,
          builtin_fun("sort"),
          tuple([
            list_lit([int(1)]),
            fn(
              Pat.tuple([Pat.var("x"), Pat.var("y")]),
              bin_op(Int(Minus), var("x"), var("y")),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    // test_case("sort sorted list of 2 numbers", `Quick, () =>
    //   evaluation_test(
    //     "sort([1, 2], fun (x, y) -> x - y)",
    //     list_lit([int(1), int(2)]),
    //     ap(
    //       Forward,
    //       builtin_fun("sort"),
    //       tuple([
    //         list_lit([int(1), int(2)]),
    //         fn(
    //           Pat.tuple([Pat.var("x"), Pat.var("y")]),
    //           bin_op(Int(Minus), var("x"), var("y")),
    //           None,
    //           None,
    //         ),
    //       ]),
    //     ),
    //   )
    // ),
    // test_case("sort ascending numbers", `Quick, () =>
    //   evaluation_test(
    //     "sort([3, 1, 4, 1, 5], fun (x, y) -> x - y)",
    //     list_lit([int(1), int(1), int(3), int(4), int(5)]),
    //     ap(
    //       Forward,
    //       builtin_fun("sort"),
    //       tuple([
    //         list_lit([int(3), int(1), int(4), int(1), int(5)]),
    //         fn(
    //           Pat.tuple([Pat.var("x"), Pat.var("y")]),
    //           bin_op(Int(Minus), var("x"), var("y")),
    //           None,
    //           None,
    //         ),
    //       ]),
    //     ),
    //   )
    // ),
    // test_case("sort descending numbers", `Quick, () =>
    //   evaluation_test(
    //     "sort([3, 1, 4, 1, 5], fun (x, y) -> y - x)",
    //     list_lit([int(5), int(4), int(3), int(1), int(1)]),
    //     ap(
    //       Forward,
    //       builtin_fun("sort"),
    //       tuple([
    //         list_lit([int(3), int(1), int(4), int(1), int(5)]),
    //         fn(
    //           Pat.tuple([Pat.var("x"), Pat.var("y")]),
    //           bin_op(Int(Minus), var("y"), var("x")),
    //           None,
    //           None,
    //         ),
    //       ]),
    //     ),
    //   )
    // ),
    // test_case("sort by absolute value", `Quick, () =>
    //   evaluation_test(
    //     "sort([-3, 1, -4, 2], fun (x, y) -> abs(x) - abs(y))",
    //     list_lit([int(1), int(2), int(-3), int(-4)]),
    //     ap(
    //       Forward,
    //       builtin_fun("sort"),
    //       tuple([
    //         list_lit([int(-3), int(1), int(-4), int(2)]),
    //         fn(
    //           Pat.tuple([Pat.var("x"), Pat.var("y")]),
    //           bin_op(
    //             Int(Minus),
    //             ap(Forward, builtin_fun("abs"), var("x")),
    //             ap(Forward, builtin_fun("abs"), var("y")),
    //           ),
    //           None,
    //           None,
    //         ),
    //       ]),
    //     ),
    //   )
    // ),
    test_case("filter_map with Some values", `Quick, () =>
      evaluation_test(
        "filter_map([1, 2, 3, 4], fun x -> if int_mod(x, 2) == 0 then Some(x * 2) else None)",
        list_lit([int(4), int(8)]),
        ap(
          Forward,
          builtin_fun("filter_map"),
          tuple([
            list_lit([int(1), int(2), int(3), int(4)]),
            fn(
              Pat.var("x"),
              if_(
                bin_op(
                  Int(Equals),
                  ap(
                    Forward,
                    builtin_fun("int_mod"),
                    tuple([var("x"), int(2)]),
                  ),
                  int(0),
                ),
                ap(Forward, some, bin_op(Int(Times), var("x"), int(2))),
                none,
              ),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("nth_opt valid index", `Quick, () =>
      evaluation_test(
        "nth_opt([1, 2, 3], 1)",
        ap(Forward, some, int(2)),
        ap(
          Forward,
          builtin_fun("nth_opt"),
          tuple([list_lit([int(1), int(2), int(3)]), int(1)]),
        ),
      )
    ),
    test_case("nth_opt invalid index", `Quick, () =>
      evaluation_test(
        "nth_opt([1, 2, 3], 5)",
        none,
        ap(
          Forward,
          builtin_fun("nth_opt"),
          tuple([list_lit([int(1), int(2), int(3)]), int(5)]),
        ),
      )
    ),
    test_case("find_opt found element", `Quick, () =>
      evaluation_test(
        "find_opt([1, 2, 3, 4], fun x -> int_mod(x, 2) == 0)",
        ap(Forward, some, int(2)),
        ap(
          Forward,
          builtin_fun("find_opt"),
          tuple([
            list_lit([int(1), int(2), int(3), int(4)]),
            fn(
              Pat.var("x"),
              bin_op(
                Int(Equals),
                ap(
                  Forward,
                  builtin_fun("int_mod"),
                  tuple([var("x"), int(2)]),
                ),
                int(0),
              ),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("find_opt not found", `Quick, () =>
      evaluation_test(
        "find_opt([1, 3, 5], fun x -> int_mod(x, 2) == 0)",
        none,
        ap(
          Forward,
          builtin_fun("find_opt"),
          tuple([
            list_lit([int(1), int(3), int(5)]),
            fn(
              Pat.var("x"),
              bin_op(
                Int(Equals),
                ap(
                  Forward,
                  builtin_fun("int_mod"),
                  tuple([var("x"), int(2)]),
                ),
                int(0),
              ),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("find_index found element", `Quick, () =>
      evaluation_test(
        "find_index([1, 2, 3, 4], fun x -> int_mod(x, 2) == 0)",
        ap(Forward, some, int(1)),
        ap(
          Forward,
          builtin_fun("find_index"),
          tuple([
            list_lit([int(1), int(2), int(3), int(4)]),
            fn(
              Pat.var("x"),
              bin_op(
                Int(Equals),
                ap(
                  Forward,
                  builtin_fun("int_mod"),
                  tuple([var("x"), int(2)]),
                ),
                int(0),
              ),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("find_index not found", `Quick, () =>
      evaluation_test(
        "find_index([1, 3, 5], fun x -> int_mod(x, 2) == 0)",
        none,
        ap(
          Forward,
          builtin_fun("find_index"),
          tuple([
            list_lit([int(1), int(3), int(5)]),
            fn(
              Pat.var("x"),
              bin_op(
                Int(Equals),
                ap(
                  Forward,
                  builtin_fun("int_mod"),
                  tuple([var("x"), int(2)]),
                ),
                int(0),
              ),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("find_map found element", `Quick, () =>
      evaluation_test(
        "find_map([1, 2, 3, 4], fun x -> if int_mod(x, 2) == 0 then Some(x * 2) else None)",
        ap(Forward, some, int(4)),
        ap(
          Forward,
          builtin_fun("find_map"),
          tuple([
            list_lit([int(1), int(2), int(3), int(4)]),
            fn(
              Pat.var("x"),
              if_(
                bin_op(
                  Int(Equals),
                  ap(
                    Forward,
                    builtin_fun("int_mod"),
                    tuple([var("x"), int(2)]),
                  ),
                  int(0),
                ),
                ap(Forward, some, bin_op(Int(Times), var("x"), int(2))),
                none,
              ),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("find_map not found", `Quick, () =>
      evaluation_test(
        "find_map([1, 3, 5], fun x -> if int_mod(x, 2) == 0 then Some(x * 2) else None)",
        none,
        ap(
          Forward,
          builtin_fun("find_map"),
          tuple([
            list_lit([int(1), int(3), int(5)]),
            fn(
              Pat.var("x"),
              if_(
                bin_op(
                  Int(Equals),
                  ap(
                    Forward,
                    builtin_fun("int_mod"),
                    tuple([var("x"), int(2)]),
                  ),
                  int(0),
                ),
                ap(Forward, some, bin_op(Int(Times), var("x"), int(2))),
                none,
              ),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("find_mapi found element", `Quick, () =>
      evaluation_test(
        "find_mapi([1, 2, 3, 4], fun (i, x) -> if int_mod(x, 2) == 0 then Some(i) else None)",
        ap(Forward, some, int(1)),
        ap(
          Forward,
          builtin_fun("find_mapi"),
          tuple([
            list_lit([int(1), int(2), int(3), int(4)]),
            fn(
              Pat.tuple([Pat.var("i"), Pat.var("x")]),
              if_(
                bin_op(
                  Int(Equals),
                  ap(
                    Forward,
                    builtin_fun("int_mod"),
                    tuple([var("x"), int(2)]),
                  ),
                  int(0),
                ),
                ap(Forward, some, var("i")),
                none,
              ),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
    test_case("find_mapi not found", `Quick, () =>
      evaluation_test(
        "find_mapi([1, 3, 5], fun (i, x) -> if int_mod(x, 2) == 0 then Some(i) else None)",
        none,
        ap(
          Forward,
          builtin_fun("find_mapi"),
          tuple([
            list_lit([int(1), int(3), int(5)]),
            fn(
              Pat.tuple([Pat.var("i"), Pat.var("x")]),
              if_(
                bin_op(
                  Int(Equals),
                  ap(
                    Forward,
                    builtin_fun("int_mod"),
                    tuple([var("x"), int(2)]),
                  ),
                  int(0),
                ),
                ap(Forward, some, var("i")),
                none,
              ),
              None,
              None,
            ),
          ]),
        ),
      )
    ),
  ],
);
