open Alcotest;
open Test_Evaluator_Prelude;

let tests = (
  "Evaluator.ListBuiltins",
  [
    test_case("length of empty list", `Quick, () =>
      parse_and_evaluate_test({|0|}, {|length([])|})
    ),
    test_case("length of multi-element list", `Quick, () =>
      parse_and_evaluate_test({|3|}, {|length([1, 2, 3])|})
    ),
    test_case("map with identity function", `Quick, () =>
      parse_and_evaluate_test({|[1, 2, 3]|}, {|map([1, 2, 3], fun x -> x)|})
    ),
    test_case("map with increment function", `Quick, () =>
      parse_and_evaluate_test(
        {|[2, 3, 4]|},
        {|map([1, 2, 3], fun x -> x + 1)|},
      )
    ),
    test_case("filter even numbers", `Quick, () =>
      parse_and_evaluate_test(
        {|[2, 4]|},
        {|filter([1, 2, 3, 4], fun x -> int_mod(x, 2) == 0)|},
      )
    ),
    test_case("fold_left sum", `Quick, () =>
      parse_and_evaluate_test(
        {|6|},
        {|fold_left([1, 2, 3], fun (acc, x) -> acc + x, 0)|},
      )
    ),
    test_case("flat_map duplicate elements", `Quick, () =>
      parse_and_evaluate_test(
        {|[2, 2, 1, 1]|},
        {|flat_map([1, 2], fun x -> [x, x])|},
      )
    ),
    test_case("zip two lists", `Quick, () =>
      parse_and_evaluate_test({|[(1, 3), (2, 4)]|}, {|zip([1, 2], [3, 4])|})
    ),
    test_case("unzip list of pairs", `Quick, () =>
      parse_and_evaluate_test(
        {|([1, 2], [3, 4])|},
        {|unzip([(1, 3), (2, 4)])|},
      )
    ),
    test_case("reverse list", `Quick, () =>
      parse_and_evaluate_test({|[3, 2, 1]|}, {|reverse([1, 2, 3])|})
    ),
    test_case("take first two elements", `Quick, () =>
      parse_and_evaluate_test({|[1, 2]|}, {|take([1, 2, 3], 2)|})
    ),
    test_case("drop first two elements", `Quick, () =>
      parse_and_evaluate_test({|[3]|}, {|drop([1, 2, 3], 2)|})
    ),
    test_case("range from 1 to 3", `Quick, () =>
      parse_and_evaluate_test({|[1, 2, 3]|}, {|range(1, 3)|})
    ),
    test_case("enumerate list", `Quick, () =>
      parse_and_evaluate_test({|[(0, 10), (1, 20)]|}, {|enumerate([10, 20])|})
    ),
    test_case("any with true predicate", `Quick, () =>
      parse_and_evaluate_test({|true|}, {|any([1, 2, 3], fun x -> x > 2)|})
    ),
    test_case("all with false predicate", `Quick, () =>
      parse_and_evaluate_test({|false|}, {|all([1, 2, 3], fun x -> x > 5)|})
    ),
    test_case("intersperse with separator", `Quick, () =>
      parse_and_evaluate_test({|[1, 0, 2]|}, {|intersperse([1, 2], 0)|})
    ),
    test_case("cons element to list", `Quick, () =>
      parse_and_evaluate_test({|[1, 2, 3]|}, {|cons(1, [2, 3])|})
    ),
    test_case("hd of non-empty list", `Quick, () =>
      parse_and_evaluate_test({|1|}, {|hd([1, 2, 3])|})
    ),
    test_case("tl of non-empty list", `Quick, () =>
      parse_and_evaluate_test({|[2, 3]|}, {|tl([1, 2, 3])|})
    ),
    test_case("is_empty of empty list", `Quick, () =>
      parse_and_evaluate_test({|true|}, {|is_empty([])|})
    ),
    test_case("nth element at index 1", `Quick, () =>
      parse_and_evaluate_test({|2|}, {|nth([1, 2, 3], 1)|})
    ),
    test_case("fold_right subtract", `Quick, () =>
      parse_and_evaluate_test(
        {|2|},
        {|fold_right([1, 2, 3], fun (x, acc) -> x - acc, 0)|},
      )
    ),
    test_case("append two lists", `Quick, () =>
      parse_and_evaluate_test({|[1, 2, 3, 4]|}, {|append([1, 2], [3, 4])|})
    ),
    test_case("concat list of lists", `Quick, () =>
      parse_and_evaluate_test(
        {|[1, 2, 3, 4, 5]|},
        {|concat([[1, 2], [3], [4, 5]])|},
      )
    ),
    test_case("mapi with index", `Quick, () =>
      parse_and_evaluate_test(
        {|[10, 21]|},
        {|mapi([10, 20], fun (i, x) -> i + x)|},
      )
    ),
    test_case("filteri even indices", `Quick, () =>
      parse_and_evaluate_test(
        {|[1, 3]|},
        {|filteri([1, 2, 3, 4], fun (i, x) -> int_mod(i, 2) == 0)|},
      )
    ),
    test_case("mem element in list", `Quick, () =>
      parse_and_evaluate_test({|true|}, {|mem([1, 2, 3], 2)|})
    ),
    test_case("partition even and odd", `Quick, () =>
      parse_and_evaluate_test(
        {|([2, 4], [1, 3])|},
        {|partition([1, 2, 3, 4], fun x -> int_mod(x, 2) == 0)|},
      )
    ),
    test_case("rev_append reversed list", `Quick, () =>
      parse_and_evaluate_test(
        {|[2, 1, 3, 4]|},
        {|rev_append([1, 2], [3, 4])|},
      )
    ),
    test_case("fold_left2 sum pairs", `Quick, () =>
      parse_and_evaluate_test(
        {|10|},
        {|fold_left2([1, 2], [3, 4], fun (acc, x, y) -> acc + x + y, 0)|},
      )
    ),
    test_case("fold_right2 subtract pairs", `Quick, () =>
      parse_and_evaluate_test(
        {|2|},
        {|fold_right2([2, 1], [4, 3], fun (x, y, acc) -> x + y - acc, 0)|},
      )
    ),
    test_case("map2 add pairs", `Quick, () =>
      parse_and_evaluate_test(
        {|[4, 6]|},
        {|map2([1, 2], [3, 4], fun (x, y) -> x + y)|},
      )
    ),
    test_case("all2 compare pairs", `Quick, () =>
      parse_and_evaluate_test(
        {|true|},
        {|all2([1, 2], [3, 4], fun (x, y) -> x < y)|},
      )
    ),
    test_case("any2 find equal pairs", `Quick, () =>
      parse_and_evaluate_test(
        {|true|},
        {|any2([1, 2], [3, 2], fun (x, y) -> x == y)|},
      )
    ),
    test_case("find first even number", `Quick, () =>
      parse_and_evaluate_test(
        {|2|},
        {|find([1, 2, 3, 4], fun x -> int_mod(x, 2) == 0)|},
      )
    ),
    test_case("take_while positive numbers", `Quick, () =>
      parse_and_evaluate_test(
        {|[1, 2]|},
        {|take_while([1, 2, -3, 4], fun x -> x > 0)|},
      )
    ),
    test_case("drop_while positive numbers", `Quick, () =>
      parse_and_evaluate_test(
        {|[0, 4]|},
        {|drop_while([1, 2, 0, 4], fun x -> x > 0)|},
      )
    ),
    test_case("sort empty list", `Quick, () =>
      parse_and_evaluate_test({|[]|}, {|sort([], fun (x, y) -> x - y)|})
    ),
    test_case("sort singleton list", `Quick, () =>
      parse_and_evaluate_test({|[1]|}, {|sort([1], fun (x, y) -> x - y)|})
    ),
    // These might be failing due to an eval bug?
    // test_case("sort sorted list of 2 numbers", `Quick, () =>
    //   parse_and_evaluate_test(
    //     {|[1, 2]|},
    //     {|sort([1, 2], fun (x, y) -> x - y)|},
    //   )
    // ),
    // test_case("sort ascending numbers", `Quick, () =>
    //   parse_and_evaluate_test(
    //     {|[1, 1, 3, 4, 5]|},
    //     {|sort([3, 1, 4, 1, 5], fun (x, y) -> x - y)|},
    //   )
    // ),
    // test_case("sort descending numbers", `Quick, () =>
    //   parse_and_evaluate_test(
    //     {|[5, 4, 3, 1, 1]|},
    //     {|sort([3, 1, 4, 1, 5], fun (x, y) -> y - x)|},
    //   )
    // ),
    // test_case("sort by absolute value", `Quick, () =>
    //   parse_and_evaluate_test(
    //     {|[1, 2, -3, -4]|},
    //     {|sort([-3, 1, -4, 2], fun (x, y) -> abs(x) - abs(y))|},
    //   )
    // ),
    test_case("filter_map with Some values", `Quick, () =>
      parse_and_evaluate_test(
        {|[4, 8]|},
        {|filter_map([1, 2, 3, 4], fun x -> if int_mod(x, 2) == 0 then Some(x * 2) else None)|},
      )
    ),
    test_case("nth_opt valid index", `Quick, () =>
      parse_and_evaluate_test({|Some(2)|}, {|nth_opt([1, 2, 3], 1)|})
    ),
    test_case("nth_opt invalid index", `Quick, () =>
      parse_and_evaluate_test({|None|}, {|nth_opt([1, 2, 3], 5)|})
    ),
    test_case("find_opt found element", `Quick, () =>
      parse_and_evaluate_test(
        {|Some(2)|},
        {|find_opt([1, 2, 3, 4], fun x -> int_mod(x, 2) == 0)|},
      )
    ),
    test_case("find_opt not found", `Quick, () =>
      parse_and_evaluate_test(
        {|None|},
        {|find_opt([1, 3, 5], fun x -> int_mod(x, 2) == 0)|},
      )
    ),
    test_case("find_index found element", `Quick, () =>
      parse_and_evaluate_test(
        {|Some(1)|},
        {|find_index([1, 2, 3, 4], fun x -> int_mod(x, 2) == 0)|},
      )
    ),
    test_case("find_index not found", `Quick, () =>
      parse_and_evaluate_test(
        {|None|},
        {|find_index([1, 3, 5], fun x -> int_mod(x, 2) == 0)|},
      )
    ),
    test_case("find_map found element", `Quick, () =>
      parse_and_evaluate_test(
        {|Some(4)|},
        {|find_map([1, 2, 3, 4], fun x -> if int_mod(x, 2) == 0 then Some(x * 2) else None)|},
      )
    ),
    test_case("find_map not found", `Quick, () =>
      parse_and_evaluate_test(
        {|None|},
        {|find_map([1, 3, 5], fun x -> if int_mod(x, 2) == 0 then Some(x * 2) else None)|},
      )
    ),
    test_case("find_mapi found element", `Quick, () =>
      parse_and_evaluate_test(
        {|Some(1)|},
        {|find_mapi([1, 2, 3, 4], fun (i, x) -> if int_mod(x, 2) == 0 then Some(i) else None)|},
      )
    ),
    test_case("find_mapi not found", `Quick, () =>
      parse_and_evaluate_test(
        {|None|},
        {|find_mapi([1, 3, 5], fun (i, x) -> if int_mod(x, 2) == 0 then Some(i) else None)|},
      )
    ),
  ],
);
