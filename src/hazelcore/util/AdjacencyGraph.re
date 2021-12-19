include AdjacencyMap.Make(IntMap, IntSet);

open Sexplib.Std;

[@deriving sexp]
type binding = (int, list(int));

let bindings = (graph: t): list(binding) =>
  graph |> bindings |> List.map(TupleUtil.map_right(IntSet.elements));

let of_list = (bindings: list(binding)): t => {
  let union = ys =>
    fun
    | None => Some(ys)
    | Some(zs) => Some(IntSet.union(ys, zs));
  bindings
  |> List.map(TupleUtil.map_right(IntSet.of_list))
  |> List.fold_left((adj, (x, ys)) => adj |> update(x, union(ys)), empty);
};

let sexp_of_t = (graph: t): Sexplib.Sexp.t =>
  graph |> bindings |> sexp_of_list(sexp_of_binding);

let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
  sexp |> list_of_sexp(binding_of_sexp) |> of_list;
