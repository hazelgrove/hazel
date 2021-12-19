open Sexplib.Std;

[@deriving sexp]
type part =
  | U
  | V;

module IntPart = {
  [@deriving sexp]
  type t = (int, part);
  let compare = compare;
};

module IntPartMap = {
  include Map.Make(IntPart);

  open Sexplib.Std;

  [@deriving sexp]
  type binding('a) = (IntPart.t, 'a);

  let of_list = (bindings: list(binding('a))): t('a) =>
    bindings |> List.to_seq |> of_seq;

  let sexp_of_t =
      (sexp_of_v: 'v => Sexplib.Sexp.t, map: t('v)): Sexplib.Sexp.t =>
    map |> bindings |> sexp_of_list(sexp_of_binding(sexp_of_v));

  let t_of_sexp =
      (v_of_sexp: Sexplib.Sexp.t => 'v, sexp: Sexplib.Sexp.t): t('v) =>
    sexp |> list_of_sexp(binding_of_sexp(v_of_sexp)) |> of_list;
};

module IntPartSet = {
  include Set.Make(IntPart);

  open Sexplib.Std;

  let add' = (part: part, x: int): (t => t) => add((x, part));

  let of_set = (part: part, xs: IntSet.t): t =>
    empty |> IntSet.fold(add'(part), xs);

  let sexp_of_t = (set: t): Sexplib.Sexp.t =>
    set |> elements |> sexp_of_list(IntPart.sexp_of_t);

  let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
    sexp |> list_of_sexp(IntPart.t_of_sexp) |> of_list;
};

include AdjacencyMap.Make(IntPartMap, IntPartSet);

let of_list = (bindings: list(binding)): t => {
  let union = ys =>
    fun
    | None => Some(ys)
    | Some(zs) => Some(IntPartSet.union(ys, zs));
  let merge_sets = (adj, (x, ys)) => adj |> update(x, union(ys));
  bindings |> List.fold_left(merge_sets, empty);
};

let transpose = (alt: t): t => {
  let transpose_binding = ((u, vs): binding): list(binding) =>
    vs |> IntPartSet.elements |> List.map(v => (v, IntPartSet.singleton(u)));
  alt |> bindings |> List.map(transpose_binding) |> List.concat |> of_list;
};

module Path = {
  type alt = t;

  let sexp_of_alt = sexp_of_t;

  [@deriving sexp]
  type t = list(IntPart.t);

  let add = (x: IntPart.t, path: t): t => [x, ...path];

  let between =
      (root: IntPart.t, terminals: IntPartSet.t, alt: alt): option((t, alt)) => {
    open OptUtil.Syntax;
    let rec loopU = (u: IntPart.t, alt: alt): option((t, alt)) => {
      switch (u) {
      | (_, U) =>
        switch (alt |> find_opt(u)) {
        | None =>
          terminals |> IntPartSet.mem(u)
            ? Some(([u], alt |> remove_target(u))) : None
        | Some(vs) =>
          let rec inner_loopU = vs => {
            let* v = vs |> IntPartSet.min_elt_opt;
            let alt = alt |> remove_source(u) |> remove_target(u);
            switch (
              alt
              |> loopV(v)
              |> Option.map(((path, alt)) => (path |> add(u), alt))
            ) {
            | Some((path, alt)) => Some((path, alt))
            | None => inner_loopU(vs |> IntPartSet.remove(v))
            };
          };
          inner_loopU(vs);
        }
      | (_, V) => failwith(__LOC__ ++ ": invalid graph")
      };
    }
    and loopV = (v: IntPart.t, alt: alt): option((t, alt)) =>
      switch (v) {
      | (_, V) =>
        let* us = alt |> find_opt(v);
        let rec inner_loopV = us => {
          let* u = us |> IntPartSet.min_elt_opt;
          let alt = alt |> remove_source(v) |> remove_target(v);
          switch (
            alt |> loopU(u) |> Option.map(TupleUtil.map_left(add(v)))
          ) {
          | Some((path, alt)) => Some((path, alt))
          | None => inner_loopV(us |> IntPartSet.remove(u))
          };
        };
        inner_loopV(us);
      | (_, U) => failwith(__LOC__ ++ ": invalid graph")
      };
    loopV(root, alt);
  };

  let rec disjunctive_union =
          (path: t, matching: BipartiteMatching.t): BipartiteMatching.t =>
    switch (path) {
    | [(u, U), (_, V) as y, ...path'] =>
      matching
      |> BipartiteMatching.unmatch(u)
      |> disjunctive_union([y, ...path'])
    | [(v, V), (u, U) as y, ...path'] =>
      matching
      |> BipartiteMatching.match(u, v)
      |> disjunctive_union([y, ...path'])
    | [(_, U)]
    | [] => matching
    | _ => failwith(__LOC__ ++ ": invalid path")
    };
};

let vertex_disjoint_paths =
    (roots: IntPartSet.t, terminals: IntPartSet.t, alt: t): list(Path.t) => {
  let rec loop = (roots: IntPartSet.t, alt: t): (list(Path.t), t) =>
    switch (roots |> IntPartSet.min_elt_opt) {
    | None => ([], alt)
    | Some(root) =>
      let roots = roots |> IntPartSet.remove(root);
      switch (alt |> Path.between(root, terminals)) {
      | None => alt |> remove_source(root) |> loop(roots)
      | Some((path, alt)) =>
        let (paths, alt) = loop(roots, alt);
        ([path, ...paths], alt);
      };
    };
  alt |> loop(roots) |> fst;
};
