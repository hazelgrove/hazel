include (module type of AdjacencyMap.Make(IntMap, IntSet));

[@deriving sexp]
type binding = (int, list(int));

let bindings: t => list(binding);

let of_list: list(binding) => t;

let sexp_of_t: t => Sexplib.Sexp.t;

let t_of_sexp: Sexplib.Sexp.t => t;
