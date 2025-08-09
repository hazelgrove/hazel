open Language;

/* Few-shot hole completion examples as per the Static Contextualization paper */

let examples =
    (hole_label: string, advanced_reasoning: bool)
    : list((string, string, string)) => {
  let expected_type = ty =>
    RelevantTypes.get(Ctx.empty, Typ.fresh(ty), hole_label);
  [
    (
      {|
let List.length: [(String, Bool)]-> Int =
  fun xs ->
    |}
      ++ hole_label
      ++ {| end in
|},
      expected_type(Atom(Int)),
      advanced_reasoning
        ? {|
Discussion:
The function List.length takes a list of (String, Bool) tuples and returns an Int. The natural way to compute the length of a list is through recursion.
The base case for an empty list is 0, and for a non-empty list, we increment the count and recursively call List.length on the tail.
```case xs
| [] => 0
| _::xs => 1 + List.length(xs)
```|}
        : {|
case xs
| [] => 0
| _::xs => 1 + List.length(xs)|},
    ),
    (
      {|
let List.mapi: ((Int, Bool) -> Bool, [Bool]) -> [Bool]=
  fun f, xs ->
    let go: (Int, [Bool])-> [Bool] = fun idx, xs ->
      |}
      ++ hole_label
      ++ {| end in
    go(0, xs) in
|},
      expected_type(List(Typ.fresh(Atom(Bool)))),
      advanced_reasoning
        ? {|
Discussion:
The function List.mapi applies a function f to each element of a list while keeping track of the index. The helper function go does this recursively.
The base case returns an empty list. In the recursive case, f(idx, hd) is applied to the head, and go(idx + 1, tl) is called recursively on the tail to process the rest of the list.
```case xs
| [] => []
| hd::tl => f(idx, hd)::go(idx + 1, tl)
```|}
        : {|
case xs
| [] => []
| hd::tl => f(idx, hd)::go(idx + 1, tl)
|},
    ),
    (
      {|
type Container =
  + Pod(Bool)
  + CapsuleCluster(Int, Int) in
let total_capacity: Container -> Int =
  |}
      ++ hole_label
      ++ {|
in
|},
      expected_type(
        Arrow(Typ.fresh(Var("Container")), Typ.fresh(Atom(Int))),
      ),
      advanced_reasoning
        ? {|
Discussion:
The function total_capacity takes a Container and returns an Int. The Pod variant stores a Bool, which likely indicates whether the pod is active.
The condition if !b && true simplifies to if !b, meaning inactive pods have a capacity of 1, while active ones have 0.
The CapsuleCluster variant contains two integers, which are multiplied together to represent the total capacity.
```fun c ->
    case c
      | Pod(b) => if !b && true then 1 else 0
      | CapsuleCluster(x, y) => x * y
    end
```
|}
        : {|
fun c ->
    case c
      | Pod(b) => if !b && true then 1 else 0
      | CapsuleCluster(x, y) => x * y
    end
|},
    ),
    (
      "let f = " ++ hole_label ++ " in f(5)",
      expected_type(Unknown(Internal |> Prov.anonymous)),
      advanced_reasoning
        ? {|
Discussion:
The expression let f = ?a in f(5) means f should be a function that can take an integer input. A function of type fun x:Int -> ?a is defined, but its body is missing.
Since no constraints are placed on the output type, the hole could be filled with any valid expression.
```
fun x:Int -> ?a
```
      |}
        : "fun x:Int -> ??",
    ),
    (
      {|let triple = (4, 8, true) in
let (_, y, condition) = triple in
let get: Option -> Int =
fun maybe_num ->
  case maybe_num
 | Some(x) => |}
      ++ hole_label
      ++ {|
 | None => if !condition then 0 else y + 1 end in|},
      expected_type(Atom(Int)),
      advanced_reasoning
        ? {|
Discussion:
The function get extracts a value from an Option type. If Some(x), the function should return x, as x is already of type Int.
The None case considers a condition; if !condition is true, it returns 0, otherwise, it returns y + 1.
Since x is an Int, returning it in the Some case maintains type consistency.
```
x
```
      |}
        : "x",
    ),
    (
      "let num_or_zero = fun maybe_num ->\n case maybe_num\n | Some(num) => "
      ++ hole_label
      ++ " \n| None => 0 end in",
      expected_type(Unknown(Internal |> Prov.anonymous)),
      advanced_reasoning
        ? {|
Discussion:
The function num_or_zero takes an Option(Int) and returns an Int. If the input is Some(num), it should return num, as num is already an integer.
If None, the function defaults to returning 0. This ensures type consistency while preserving the stored number when available.
```
num
```
      |}
        : "num",
    ),
    (
      "let merge_sort: [Int]->[Int] =\n"
      ++ hole_label
      ++ "\nin\nmerge_sort([4,1,3,7,2])",
      expected_type(
        Arrow(
          Typ.fresh(List(Typ.fresh(Atom(Int)))),
          Typ.fresh(List(Typ.fresh(Atom(Int)))),
        ),
      ),
      advanced_reasoning
        ? {|
Discussion:
The function merge_sort sorts a list of integers. A common approach to implementing merge sort involves:
1. Splitting the list into two halves (split).
2. Recursively sorting both halves (merge_sort_helper).
3. Merging the sorted halves (merge).
The provided structure follows this approach, so we use helper functions to complete the sorting logic.
```
fun list ->\nlet split: [Int]->([Int],[Int]) = fun left, right -> ?\nin\nlet merge: ([Int],[Int])->[Int]= ?\nin\nlet merge_sort_helper: [Int]->[Int]= ?\nin\nmerge_sort_helper(list)
```
      |}
        : "fun list ->\nlet split: [Int]->([Int],[Int]) = fun left, right -> ?\nin\nlet merge: ([Int],[Int])->[Int]= ?\nin\nlet merge_sort_helper: [Int]->[Int]= ?\nin\nmerge_sort_helper(list)",
    ),
    (
      "type MenuItem =\n+ Breakfast(Int, Int)\n+ Lunch(Float)\nin\nlet per_lunch_unit = 0.95 in\nlet price: MenuItem-> Float   = fun m ->\ncase m\n| Breakfast(x, y) => "
      ++ hole_label
      ++ "\n| Lunch(f) => f *. per_lunch_unit\nend\nin price(Breakfast(1,2))/.3.",
      expected_type(Var("MenuItem")),
      advanced_reasoning
        ? {|
Discussion:
The function price computes the cost of a MenuItem. The Lunch variant already has a predefined price calculation. For Breakfast(x, y), an expression must return a Float, but the completion is missing.
The function should ensure a proper numeric computation based on x and y.
```
fun m ->\ncase m\n| Breakfast(x, y) => ?a\n| Lunch(f) => f *. per_lunch_unit\nend
```
      |}
        : "fun m ->\ncase m\n| Breakfast(x, y) => ??\n| Lunch(f) => f *. per_lunch_unit\nend",
    ),
    (
      {|
let List.merge: (( , )->Bool,[ ], [ ]) -> [ ] = fun cmp,left, right ->
case left, right
| [], _ => right
| _, [] => left
| h1::t1, h2::t2 =>
if cmp(h1, h2)
then h1 :: List.merge(cmp, t1, right)
else h2 :: List.merge(cmp,left, t2)
end
in

let List.sort: ((?, ?) -> Bool, [?]) -> [?] =
fun cmp, list ->
let merge_sort_helper: [?] -> [?] = fun l ->
case  l
| [] => ?
| [x] => [x]
| _ => |}
      ++ hole_label
      ++ {|
end
in merge_sort_helper(list)
in
test 2 == List.nth(List.sort(fun a, b -> a<b, [4,1,3,2]), 1) end
    |},
      expected_type(List(Typ.fresh(Unknown(Internal |> Prov.anonymous)))),
      advanced_reasoning
        ? {|
Discussion:
The function List.merge merges two sorted lists using a comparator function cmp. The List.sort function applies merge sort, using merge_sort_helper to recursively divide and sort the list.
The base cases return [] or a single-element list. The recursive case splits the list into two halves and merges sorted sublists.
```let mid = List.length(l) / 2 in
let left, right = List.take(mid, l), List.drop(mid, l) in
List.merge(cmp, merge_sort_helper(left), merge_sort_helper(right))
```|}
        : {|
let mid = List.length(l) / 2 in
let left, right = List.take(mid, l), List.drop(mid, l) in
List.merge(cmp, merge_sort_helper(left), merge_sort_helper(right))
|},
    ),
  ];
};

let get =
    (num_examples: int, hole_label: string, advanced_reasoning: bool)
    : list(string) =>
  Util.ListUtil.flat_map(
    ((sketch, expected_ty, completion)): list(string) =>
      [sketch] @ ["expected_ty: " ++ expected_ty] @ [completion],
    switch (
      Util.ListUtil.split_n_opt(
        num_examples,
        examples(hole_label, advanced_reasoning),
      )
    ) {
    | Some(samples) => samples |> fst |> List.map(((s, t, u)) => (s, t, u))
    | None => []
    },
  );
