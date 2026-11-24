open Alcotest;
open Test_Evaluator_Prelude;

let tests = (
  "Evaluator.ListBuiltins",
  [
    test_case("length of empty list", `Quick, () =>
      parse_and_evaluate_test({|0|}, {|List.length([])|})
    ),
    test_case("length of multi-element list", `Quick, () =>
      parse_and_evaluate_test({|3|}, {|List.length([1, 2, 3])|})
    ),
    test_case("map with identity function", `Quick, () =>
      parse_and_evaluate_test(
        {|[1, 2, 3]|},
        {|List.map([1, 2, 3], fun x -> x)|},
      )
    ),
    test_case("map with increment function", `Quick, () =>
      parse_and_evaluate_test(
        {|[2, 3, 4]|},
        {|List.map([1, 2, 3], fun x -> x + 1)|},
      )
    ),
    test_case("filter even numbers", `Quick, () =>
      parse_and_evaluate_test(
        {|[2, 4]|},
        {|List.filter([1, 2, 3, 4], fun x -> Int.mod(x, 2) == 0)|},
      )
    ),
    test_case("fold_left sum", `Quick, () =>
      parse_and_evaluate_test(
        {|6|},
        {|List.fold_left([1, 2, 3], fun (acc, x) -> acc + x, 0)|},
      )
    ),
    test_case("flat_map duplicate elements", `Quick, () =>
      parse_and_evaluate_test(
        {|[1, 1, 2, 2]|},
        {|List.flat_map([1, 2], fun x -> [x, x])|},
      )
    ),
    test_case("zip two lists", `Quick, () =>
      parse_and_evaluate_test(
        {|[(1, 3), (2, 4)]|},
        {|List.zip([1, 2], [3, 4])|},
      )
    ),
    test_case("unzip list of pairs", `Quick, () =>
      parse_and_evaluate_test(
        {|([1, 2], [3, 4])|},
        {|List.unzip([(1, 3), (2, 4)])|},
      )
    ),
    test_case("reverse list", `Quick, () =>
      parse_and_evaluate_test({|[3, 2, 1]|}, {|List.reverse([1, 2, 3])|})
    ),
    test_case("take first two elements", `Quick, () =>
      parse_and_evaluate_test({|[1, 2]|}, {|List.take([1, 2, 3], 2)|})
    ),
    test_case("drop first two elements", `Quick, () =>
      parse_and_evaluate_test({|[3]|}, {|List.drop([1, 2, 3], 2)|})
    ),
    test_case("range from 1 to 3", `Quick, () =>
      parse_and_evaluate_test({|[1, 2, 3]|}, {|List.range(1, 3)|})
    ),
    test_case("enumerate list", `Quick, () =>
      parse_and_evaluate_test(
        {|[(0, 10), (1, 20)]|},
        {|List.enumerate([10, 20])|},
      )
    ),
    test_case("any with true predicate", `Quick, () =>
      parse_and_evaluate_test(
        {|true|},
        {|List.any([1, 2, 3], fun x -> x > 2)|},
      )
    ),
    test_case("all with false predicate", `Quick, () =>
      parse_and_evaluate_test(
        {|false|},
        {|List.all([1, 2, 3], fun x -> x > 5)|},
      )
    ),
    test_case("intersperse with separator", `Quick, () =>
      parse_and_evaluate_test({|[1, 0, 2]|}, {|List.intersperse([1, 2], 0)|})
    ),
    test_case("cons element to list", `Quick, () =>
      parse_and_evaluate_test({|[1, 2, 3]|}, {|List.cons(1, [2, 3])|})
    ),
    test_case("head of non-empty list", `Quick, () =>
      parse_and_evaluate_test({|1|}, {|List.head([1, 2, 3])|})
    ),
    test_case("tl of non-empty list", `Quick, () =>
      parse_and_evaluate_test({|[2, 3]|}, {|List.tail([1, 2, 3])|})
    ),
    test_case("is_empty of empty list", `Quick, () =>
      parse_and_evaluate_test({|true|}, {|List.is_empty([])|})
    ),
    test_case("nth element at index 1", `Quick, () =>
      parse_and_evaluate_test({|2|}, {|List.nth([1, 2, 3], 1)|})
    ),
    test_case("fold_right subtract", `Quick, () =>
      parse_and_evaluate_test(
        {|2|},
        {|List.fold_right([1, 2, 3], fun (x, acc) -> x - acc, 0)|},
      )
    ),
    test_case("append two lists", `Quick, () =>
      parse_and_evaluate_test(
        {|[1, 2, 3, 4]|},
        {|List.append([1, 2], [3, 4])|},
      )
    ),
    test_case("concat list of lists", `Quick, () =>
      parse_and_evaluate_test(
        {|[1, 2, 3, 4, 5]|},
        {|List.concat([[1, 2], [3], [4, 5]])|},
      )
    ),
    test_case("mapi with index", `Quick, () =>
      parse_and_evaluate_test(
        {|[10, 21]|},
        {|List.mapi([10, 20], fun (i, x) -> i + x)|},
      )
    ),
    test_case("filteri even indices", `Quick, () =>
      parse_and_evaluate_test(
        {|[1, 3]|},
        {|List.filteri([1, 2, 3, 4], fun (i, x) -> Int.mod(i, 2) == 0)|},
      )
    ),
    test_case("mem element in list", `Quick, () =>
      parse_and_evaluate_test({|true|}, {|List.mem([1, 2, 3], 2)|})
    ),
    test_case("partition even and odd", `Quick, () =>
      parse_and_evaluate_test(
        {|([2, 4], [1, 3])|},
        {|List.partition([1, 2, 3, 4], fun x -> Int.mod(x, 2) == 0)|},
      )
    ),
    test_case("rev_append reversed list", `Quick, () =>
      parse_and_evaluate_test(
        {|[2, 1, 3, 4]|},
        {|List.rev_append([1, 2], [3, 4])|},
      )
    ),
    test_case("fold_left2 sum pairs", `Quick, () =>
      parse_and_evaluate_test(
        {|10|},
        {|List.fold_left2([1, 2], [3, 4], fun (acc, x, y) -> acc + x + y, 0)|},
      )
    ),
    test_case("fold_right2 subtract pairs", `Quick, () =>
      parse_and_evaluate_test(
        {|2|},
        {|List.fold_right2([2, 1], [4, 3], fun (x, y, acc) -> x + y - acc, 0)|},
      )
    ),
    test_case("map2 add pairs", `Quick, () =>
      parse_and_evaluate_test(
        {|[4, 6]|},
        {|List.map2([1, 2], [3, 4], fun (x, y) -> x + y)|},
      )
    ),
    test_case("all2 compare pairs", `Quick, () =>
      parse_and_evaluate_test(
        {|true|},
        {|List.all2([1, 2], [3, 4], fun (x, y) -> x < y)|},
      )
    ),
    test_case("any2 find equal pairs", `Quick, () =>
      parse_and_evaluate_test(
        {|true|},
        {|List.any2([1, 2], [3, 2], fun (x, y) -> x == y)|},
      )
    ),
    test_case("find first even number", `Quick, () =>
      parse_and_evaluate_test(
        {|2|},
        {|List.find([1, 2, 3, 4], fun x -> Int.mod(x, 2) == 0)|},
      )
    ),
    test_case("take_while positive numbers", `Quick, () =>
      parse_and_evaluate_test(
        {|[1, 2]|},
        {|List.take_while([1, 2, -3, 4], fun x -> x > 0)|},
      )
    ),
    test_case("drop_while positive numbers", `Quick, () =>
      parse_and_evaluate_test(
        {|[0, 4]|},
        {|List.drop_while([1, 2, 0, 4], fun x -> x > 0)|},
      )
    ),
    test_case("init with identity function", `Quick, () =>
      parse_and_evaluate_test({|[0, 1, 2]|}, {|List.init(3, fun i -> i)|})
    ),
    test_case("init with square function", `Quick, () =>
      parse_and_evaluate_test(
        {|[0, 1, 4, 9]|},
        {|List.init(4, fun i -> i * i)|},
      )
    ),
    test_case("init empty list", `Quick, () =>
      parse_and_evaluate_test({|[]|}, {|List.init(0, fun i -> i)|})
    ),
    test_case("slice middle of list", `Quick, () =>
      parse_and_evaluate_test(
        {|[2, 3, 4]|},
        {|List.slice(1, 4, [1, 2, 3, 4, 5])|},
      )
    ),
    test_case("slice from beginning", `Quick, () =>
      parse_and_evaluate_test(
        {|[1, 2, 3]|},
        {|List.slice(0, 3, [1, 2, 3, 4, 5])|},
      )
    ),
    test_case("slice to end", `Quick, () =>
      parse_and_evaluate_test(
        {|[3, 4, 5]|},
        {|List.slice(2, 5, [1, 2, 3, 4, 5])|},
      )
    ),
    test_case("slice empty result", `Quick, () =>
      parse_and_evaluate_test({|[]|}, {|List.slice(5, 7, [1, 2, 3, 4, 5])|})
    ),
    test_case("slice with length 0", `Quick, () =>
      parse_and_evaluate_test({|[]|}, {|List.slice(1, 1, [1, 2, 3, 4, 5])|})
    ),
    test_case("filter_map with Some values", `Quick, () =>
      parse_and_evaluate_test(
        {|[4, 8]|},
        {|List.filter_map([1, 2, 3, 4], fun x -> if Int.mod(x, 2) == 0 then Some(x * 2) else None)|},
      )
    ),
    test_case("nth_opt valid index", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|Some(2)|},
        {|List.nth_opt([1, 2, 3], 1)|},
      )
    ),
    test_case("nth_opt invalid index", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|None|},
        {|List.nth_opt([1, 2, 3], 5)|},
      )
    ),
    test_case("find_opt found element", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|Some(2)|},
        {|List.find_opt([1, 2, 3, 4], fun x -> Int.mod(x, 2) == 0)|},
      )
    ),
    test_case("find_opt not found", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|None|},
        {|List.find_opt([1, 3, 5], fun x -> Int.mod(x, 2) == 0)|},
      )
    ),
    test_case("find_index found element", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|Some(1)|},
        {|List.find_index([1, 2, 3, 4], fun x -> Int.mod(x, 2) == 0)|},
      )
    ),
    test_case("find_index not found", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|None|},
        {|List.find_index([1, 3, 5], fun x -> Int.mod(x, 2) == 0)|},
      )
    ),
    test_case("find_map found element", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|Some(4)|},
        {|List.find_map([1, 2, 3, 4], fun x -> if Int.mod(x, 2) == 0 then Some(x * 2) else None)|},
      )
    ),
    test_case("find_map not found", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|None|},
        {|List.find_map([1, 3, 5], fun x -> if Int.mod(x, 2) == 0 then Some(x * 2) else None)|},
      )
    ),
    test_case("find_mapi found element", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|Some(1)|},
        {|List.find_mapi([1, 2, 3, 4], fun (i, x) -> if Int.mod(x, 2) == 0 then Some(i) else None)|},
      )
    ),
    test_case("find_mapi not found", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|None|},
        {|List.find_mapi([1, 3, 5], fun (i, x) -> if Int.mod(x, 2) == 0 then Some(i) else None)|},
      )
    ),
    test_case("hd_opt of non-empty list", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|Some(1)|},
        {|List.hd_opt([1, 2, 3])|},
      )
    ),
    test_case("hd_opt of empty list", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|None|},
        {|List.hd_opt([])|},
      )
    ),
    test_case("tl_opt of non-empty list", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|Some([2, 3])|},
        {|List.tl_opt([1, 2, 3])|},
      )
    ),
    test_case("tl_opt of empty list", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|None|},
        {|List.tl_opt([])|},
      )
    ),
    test_case("assoc found key", `Quick, () =>
      parse_and_evaluate_test(
        {|42|},
        {|List.assoc([(1, 10), (2, 42), (3, 30)], 2)|},
      )
    ),
    test_case("assoc_opt found key", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|Some(42)|},
        {|List.assoc_opt([(1, 10), (2, 42), (3, 30)], 2)|},
      )
    ),
    test_case("assoc_opt not found", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|None|},
        {|List.assoc_opt([(1, 10), (2, 42), (3, 30)], 5)|},
      )
    ),
    test_case("mem_assoc found key", `Quick, () =>
      parse_and_evaluate_test(
        {|true|},
        {|List.mem_assoc([(1, 10), (2, 42), (3, 30)], 2)|},
      )
    ),
    test_case("mem_assoc not found", `Quick, () =>
      parse_and_evaluate_test(
        {|false|},
        {|List.mem_assoc([(1, 10), (2, 42), (3, 30)], 5)|},
      )
    ),
    test_case("remove_assoc removes first occurrence", `Quick, () =>
      parse_and_evaluate_test(
        {|[(1, 10), (3, 30)]|},
        {|List.remove_assoc([(1, 10), (2, 42), (3, 30)], 2)|},
      )
    ),
    test_case("remove_assoc removes all occurrences", `Quick, () =>
      parse_and_evaluate_test(
        {|[(1, 10), (3, 30)]|},
        {|List.remove_assoc([(1, 10), (2, 42), (2, 99), (3, 30)], 2)|},
      )
    ),
    test_case("remove_assoc key not found", `Quick, () =>
      parse_and_evaluate_test(
        {|[(1, 10), (2, 42), (3, 30)]|},
        {|List.remove_assoc([(1, 10), (2, 42), (3, 30)], 5)|},
      )
    ),
    test_case("partition_map with Left and Right values", `Quick, () =>
      parse_and_evaluate_test(
        {|([2, 4], [1, 3])|},
        {|List.partition_map([1, 2, 3, 4], fun x -> if Int.mod(x, 2) == 0 then Left(x) else Right(x))|},
      )
    ),
    test_case("partition_map with all Left values", `Quick, () =>
      parse_and_evaluate_test(
        {|([1, 2, 3], [])|},
        {|List.partition_map([1, 2, 3], fun x -> Left(x))|},
      )
    ),
    test_case("partition_map with all Right values", `Quick, () =>
      parse_and_evaluate_test(
        {|([], [1, 2, 3])|},
        {|List.partition_map([1, 2, 3], fun x -> Right(x))|},
      )
    ),
    test_case("partition_map with empty list", `Quick, () =>
      parse_and_evaluate_test(
        {|([], [])|},
        {|List.partition_map([], fun x -> Left(x))|},
      )
    ),
    test_case("sort empty list", `Quick, () =>
      parse_and_evaluate_test(
        {|[]|},
        {|List.sort(fun (x, y) -> if x == y then Eq else (if x < y then Lt else Gt), [])|},
      )
    ),
    test_case("sort singleton list", `Quick, () =>
      parse_and_evaluate_test(
        {|[1]|},
        {|List.sort(fun (x, y) -> if x == y then Eq else (if x < y then Lt else Gt), [1])|},
      )
    ),
    test_case("sort sorted list of 2 numbers", `Quick, () => {
      parse_and_evaluate_test(
        {|[1, 2]|},
        {|List.sort(fun (x, y) -> if x == y then Eq else (if x < y then Lt else Gt), [1, 2])|},
      )
    }),
    test_case("sort unsorted list of 2 numbers", `Quick, () => {
      parse_and_evaluate_test(
        {|[1, 2]|},
        {|List.sort(fun (x, y) -> if x == y then Eq else (if x < y then Lt else Gt), [2, 1])|},
      )
    }),
    test_case("sort unsorted list of 3 numbers", `Quick, () => {
      parse_and_evaluate_test(
        {|[1, 2, 3]|},
        {|List.sort(fun (x, y) -> if x == y then Eq else (if x < y then Lt else Gt), [1, 3, 2])|},
      )
    }),
    test_case("sort ascending", `Quick, () => {
      parse_and_evaluate_test(
        {|[1, 1, 3, 4, 5]|},
        {|List.sort(fun (x, y) -> if x == y then Eq else (if x < y then Lt else Gt), [3, 1, 4, 1, 5])|},
      )
    }),
    test_case("sort descending", `Quick, () => {
      parse_and_evaluate_test(
        {|[5, 4, 3, 1, 1]|},
        {|List.sort(fun (x, y) -> if x == y then Eq else (if x < y then Gt else Lt), [3, 1, 4, 1, 5])|},
      )
    }),
    test_case("unique", `Quick, () =>
      parse_and_evaluate_test(
        {|[1, 2, 3]|},
        {|List.unique([1, 2, 2, 3, 1, 3])|},
      )
    ),
    test_case("group_on_key by parity", `Quick, () =>
      parse_and_evaluate_test(
        {|[(1, [1, 3]), (0, [2, 4])]|},
        {|List.group_on_key([1, 2, 3, 4], fun x -> Int.mod(x, 2))|},
      )
    ),
    test_case("pivot_table", `Quick, () =>
      parse_and_evaluate_test(
        {|
      [(index=true, `A`=60, `B`=30, `C`=0), (index=false, `A`=20, `B`=40, `C`=60)]
      |},
        {|List.pivot_table(
  [
    ("A", true, 10),
    ("A", false, 20),
    ("B", true, 30),
    ("B", false, 40),
    ("A", true, 50),
    ("C", false, 60)
  ],
  fun (r, _, _) -> r,
  fun (_, c, _) -> c,
  List.fold_left(_, fun (acc, (_, _, v)) -> acc + v, 0)
)
|},
      )
    ),
  ],
);
