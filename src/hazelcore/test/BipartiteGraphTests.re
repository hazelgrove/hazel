open BipartiteGraph;

let init = (partU: IntSet.t, partV: IntSet.t, adj: AdjacencyGraph.t): t => {
  partU,
  partV,
  adj,
};

let check = (expected: list(M.binding), graph: t): bool => {
  let result = graph |> maximum_cardinality_matching |> M.bindings;
  // print_endline(
  //   "GOT "
  //   ++ Sexplib.Sexp.to_string_hum(
  //        Sexplib.Std.sexp_of_list(M.sexp_of_binding, result),
  //      ),
  // );
  result == expected;
};

/* ************************************************************************** */

let%test "n=0, m=0" = empty |> check([]);

let%test_module "n=1, m=0" =
  (module
   {
     ();
     let%test _ =
       init(IntSet.empty, IntSet.of_list([1]), AdjacencyGraph.empty)
       |> check([]);
     let%test _ = of_list([1 --> []]) |> check([(1, Unmatched)]);
     ();
   });

let%test_module "n=2, m=0" =
  (module
   {
     ();
     let%test _ =
       init(
         IntSet.of_list([1]),
         IntSet.of_list([2]),
         AdjacencyGraph.of_list([1 --> []]),
       )
       |> check([(1, Unmatched)]);
     let%test _ =
       of_list([1 --> [], 2 --> []])
       |> check([(1, Unmatched), (2, Unmatched)]);
     ();
   });

let%test "n=2, m=1" = of_list([1 --> [2]]) |> check([(1, Matched(2))]);

let%test_module "n=3, m=0" =
  (module
   {
     ();
     let%test _ =
       init(
         IntSet.of_list([1, 2]),
         IntSet.of_list([3]),
         AdjacencyGraph.of_list([1 --> [], 2 --> []]),
       )
       |> check([(1, Unmatched), (2, Unmatched)]);
     let%test _ =
       init(
         IntSet.of_list([1, 2]),
         IntSet.of_list([3]),
         AdjacencyGraph.of_list([1 --> [], 2 --> []]),
       )
       |> check([(1, Unmatched), (2, Unmatched)]);
     ();
   });

let%test_module "n=3, m=1" =
  (module
   {
     ();
     let%test _ =
       of_list([1 --> [3], 2 --> []])
       |> check([(1, Matched(3)), (2, Unmatched)]);
     let%test _ =
       of_list([1 --> [], 2 --> [3]])
       |> check([(1, Unmatched), (2, Matched(3))]);
     let%test _ =
       init(
         IntSet.of_list([1]),
         IntSet.of_list([2, 3]),
         AdjacencyGraph.of_list([1 --> [2]]),
       )
       |> check([(1, Matched(2))]);
     let%test _ =
       init(
         IntSet.of_list([1]),
         IntSet.of_list([2, 3]),
         AdjacencyGraph.of_list([1 --> [3]]),
       )
       |> check([(1, Matched(3))]);
     ();
   });

let%test_module "n=3, m=2" =
  (module
   {
     ();
     let%test _ =
       of_list([1 --> [3], 2 --> [3]])
       |> check([(1, Matched(3)), (2, Unmatched)]);
     let%test _ = of_list([1 --> [2, 3]]) |> check([(1, Matched(2))]);
     ();
   });

let%test "n=4, m=0" =
  init(
    IntSet.of_list([1, 2]),
    IntSet.of_list([3, 4]),
    AdjacencyGraph.of_list([1 --> [], 2 --> []]),
  )
  |> check([(1, Unmatched), (2, Unmatched)]);

let%test_module "n=4, m=1" =
  (module
   {
     ();
     let%test _ =
       init(
         IntSet.of_list([1, 2]),
         IntSet.of_list([3, 4]),
         AdjacencyGraph.of_list([1 --> [3], 2 --> []]),
       )
       |> check([(1, Matched(3)), (2, Unmatched)]);
     let%test _ =
       init(
         IntSet.of_list([1, 2]),
         IntSet.of_list([3, 4]),
         AdjacencyGraph.of_list([1 --> [4], 2 --> []]),
       )
       |> check([(1, Matched(4)), (2, Unmatched)]);
     let%test _ =
       init(
         IntSet.of_list([1, 2]),
         IntSet.of_list([3, 4]),
         AdjacencyGraph.of_list([1 --> [], 2 --> [3]]),
       )
       |> check([(1, Unmatched), (2, Matched(3))]);
     let%test _ =
       init(
         IntSet.of_list([1, 2]),
         IntSet.of_list([3, 4]),
         AdjacencyGraph.of_list([1 --> [], 2 --> [4]]),
       )
       |> check([(1, Unmatched), (2, Matched(4))]);
     ();
   });

let%test_module "n=4, m=2" =
  (module
   {
     ();
     let%test _ =
       of_list([1 --> [3, 4], 2 --> []])
       |> check([(1, Matched(3)), (2, Unmatched)]);
     let%test _ =
       of_list([1 --> [3], 2 --> [4]])
       |> check([(1, Matched(3)), (2, Matched(4))]);
     let%test _ =
       of_list([1 --> [], 2 --> [3, 4]])
       |> check([(1, Unmatched), (2, Matched(3))]);
     let%test _ =
       of_list([1 --> [4], 2 --> [3]])
       |> check([(1, Matched(4)), (2, Matched(3))]);
     let%test _ =
       init(
         IntSet.of_list([1, 2]),
         IntSet.of_list([3, 4]),
         AdjacencyGraph.of_list([1 --> [3], 2 --> [3]]),
       )
       |> check([(1, Matched(3)), (2, Unmatched)]);
     let%test _ =
       init(
         IntSet.of_list([1, 2]),
         IntSet.of_list([3, 4]),
         AdjacencyGraph.of_list([1 --> [4], 2 --> [4]]),
       )
       |> check([(1, Matched(4)), (2, Unmatched)]);
     ();
   });

let%test_module "n=4, m=3" =
  (module
   {
     ();
     let%test _ =
       of_list([1 --> [3, 4], 2 --> [3]])
       |> check([(1, Matched(4)), (2, Matched(3))]);
     let%test _ =
       of_list([1 --> [3], 2 --> [3, 4]])
       |> check([(1, Matched(3)), (2, Matched(4))]);
     let%test _ =
       of_list([1 --> [3, 4], 2 --> [3, 4]])
       |> check([(1, Matched(3)), (2, Matched(4))]);
     ();
   });

let%test "n=4, m=4" =
  of_list([1 --> [3, 4], 2 --> [3, 4]])
  |> check([(1, Matched(3)), (2, Matched(4))]);

let%test _ =
  of_list([0 --> [4, 5], 1 --> [4, 5], 2 --> [5, 6], 3 --> [7]])
  |> check([
       (0, Matched(4)),
       (1, Matched(5)),
       (2, Matched(6)),
       (3, Matched(7)),
     ]);

let%test _ =
  of_list([
    1 --> [8, 12],
    2 --> [14, 10, 13],
    3 --> [9, 12, 11],
    4 --> [14, 9],
    5 --> [14, 13, 12],
    6 --> [10, 13],
    7 --> [13, 14],
  ])
  |> check([
       (1, Matched(8)),
       (2, Matched(10)),
       (3, Matched(11)),
       (4, Matched(9)),
       (5, Matched(12)),
       (6, Matched(13)),
       (7, Matched(14)),
     ]);
