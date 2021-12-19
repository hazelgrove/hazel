[@deriving sexp]
type part =
  | U
  | V;

module IntPart: {
  [@deriving sexp]
  type t = (int, part);
  let compare: (t, t) => int;
};

module IntPartMap: {
  include Map.S with type key = IntPart.t;

  [@deriving sexp]
  type binding('a) = (IntPart.t, 'a);

  let of_list: list(binding('a)) => t('a);

  let sexp_of_t: ('a => Sexplib.Sexp.t, t('a)) => Sexplib.Sexp.t;

  let t_of_sexp: (Sexplib.Sexp.t => 'a, Sexplib.Sexp.t) => t('a);
};

module IntPartSet: {
  include Set.S with type elt = IntPart.t;

  let add': (part, int, t) => t;

  let of_set: (part, IntSet.t) => t;

  let sexp_of_t: t => Sexplib.Sexp.t;

  let t_of_sexp: Sexplib.Sexp.t => t;
};

include (module type of AdjacencyMap.Make(IntPartMap, IntPartSet));

let of_list: list(binding) => t;

let transpose: t => t;

module Path: {
  type alt = t;

  let sexp_of_alt: alt => Sexplib.Sexp.t;

  [@deriving sexp]
  type t = list(IntPart.t);

  let add: (IntPart.t, t) => t;

  let between: (IntPart.t, IntPartSet.t, alt) => option((t, alt));

  let disjunctive_union: (t, BipartiteMatching.t) => BipartiteMatching.t;
};

let vertex_disjoint_paths: (IntPartSet.t, IntPartSet.t, t) => list(Path.t);
