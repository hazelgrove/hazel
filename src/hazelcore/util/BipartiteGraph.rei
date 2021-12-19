[@deriving sexp]
type t = {
  partU: IntSet.t,
  partV: IntSet.t,
  adj: AdjacencyGraph.t,
};

let empty: t;

let add: (int, int, t) => t;

let of_list: list(AdjacencyGraph.binding) => t;

let bindings: t => list(AdjacencyGraph.binding);

let (-->): ('a, 'b) => ('a, 'b);

module G: (module type of AlternatingLevelGraph);
module M: (module type of BipartiteMatching);

let alternating_level_graph: (M.t, t) => G.t;

let maximum_cardinality_matching: t => M.t;
