[@deriving sexp]
type t = {
  partU: IntSet.t,
  partV: IntSet.t,
  adj: AdjacencyGraph.t,
};

let empty: t = {
  let partU = IntSet.empty;
  let partV = IntSet.empty;
  let adj = AdjacencyGraph.empty;
  {partU, partV, adj};
};

let add = (u: int, v: int, {partU, partV, adj}: t): t => {
  let partU = partU |> IntSet.add(u);
  let partV = partV |> IntSet.add(v);
  let adj = adj |> AdjacencyGraph.add(u, v);
  {partU, partV, adj};
};

let of_list = (bindings: list(AdjacencyGraph.binding)): t => {
  let partU = bindings |> List.map(fst) |> IntSet.of_list;
  let partV = bindings |> List.map(snd) |> List.concat |> IntSet.of_list;
  let adj = AdjacencyGraph.of_list(bindings);
  {partU, partV, adj};
};

let bindings = ({adj, _}: t): list(AdjacencyGraph.binding) =>
  adj |> AdjacencyGraph.bindings;

let (-->) = (a: 'a, b: 'b): ('a, 'b) => (a, b);

module G = AlternatingLevelGraph;
module M = BipartiteMatching;

let alternating_level_graph = (matching: M.t, {partV, adj, _}: t): G.t => {
  // potential roots of shortest augmenting paths
  let initialU = matching |> M.unmatched_sources |> G.IntPartSet.of_set(U);
  // potential leaves of shortest augmenting paths
  let finalV =
    matching |> M.unmatched_targets(partV) |> G.IntPartSet.of_set(V);
  let next_node =
      (
        (level: list(G.IntPart.t), alt: G.t, seen: G.IntPartSet.t),
        x: G.IntPart.t,
      )
      : (list(G.IntPart.t), G.t, G.IntPartSet.t) => {
    let update = (x: G.IntPart.t, ys: G.IntPartSet.t) => (
      level @ (ys |> G.IntPartSet.elements),
      alt |> G.IntPartSet.fold(G.add(x), ys),
      seen |> G.IntPartSet.add(x),
    );
    let ys =
      switch (x) {
      | (u, U) =>
        switch (adj |> AdjacencyGraph.find_opt(u)) {
        | None => failwith(__LOC__ ++ ": invalid node id")
        | Some(vs) =>
          let adjacentV = vs |> G.IntPartSet.of_set(V);
          let unseenV = G.IntPartSet.diff(adjacentV, seen);
          let matchedV =
            matching |> M.matched_sources |> G.IntPartSet.of_set(V);
          G.IntPartSet.diff(unseenV, matchedV);
        }
      | (v, V) =>
        let sourcesU =
          matching |> M.find_sources(v) |> G.IntPartSet.of_set(U);
        G.IntPartSet.diff(sourcesU, seen);
      };
    update(x, ys);
  };
  // need to process the whole level to find all shortest augmenting paths
  let rec next_level =
          (level: list(G.IntPart.t), alt: G.t, seen: G.IntPartSet.t): G.t =>
    level == []
    || !G.IntPartSet.disjoint(level |> G.IntPartSet.of_list, finalV)
      ? alt
      : level
        |> List.fold_left(next_node, ([], alt, seen))
        |> TupleUtil.uncurry3(next_level);
  next_level(initialU |> G.IntPartSet.elements, G.empty, G.IntPartSet.empty);
};

let maximum_cardinality_matching = (graph: t): M.t => {
  let rec loop = (matching: M.t): M.t => {
    let alt: G.t = graph |> alternating_level_graph(matching);
    let altT: G.t = alt |> G.transpose;
    let paths: list(G.Path.t) = {
      let roots =
        matching
        |> M.unmatched_targets(graph.partV)
        |> G.IntPartSet.of_set(V);
      let terminals =
        matching |> M.unmatched_sources |> G.IntPartSet.of_set(U);
      altT |> G.vertex_disjoint_paths(roots, terminals);
    };
    let matching: M.t =
      paths
      |> List.fold_left(TupleUtil.flip(G.Path.disjunctive_union), matching);
    paths == [] ? matching : loop(matching);
  };
  loop(M.init(graph.partU));
};
