include Walk;

module Base = {
  type t = Walk.t(EStep.t);
};
include Base;

let empty = Chain.of_loop([]);

let of_g = Chain.map_loop(EStep.of_g);

let split_dst = w =>
  switch (Chain.fst(w)) {
  | [] => None
  | [hd, ...tl] => Some((hd, Chain.put_fst(tl, w)))
  };
let dst = w => fst(split_dst(w));

module Set = {
  // journeys indexed by their destinations (exclusive)
  type t = EStep.Map.t(list(Base.t));
  let empty = EStep.Map.empty;

  let of_g = (set: GWalk.Set.t) =>
    GWalk.Set.to_list(set)
    |> List.map(((dst, ws)) => (EStep.of_g(dst), List.map(of_g, ws)))
    |> EStep.Map.of_list;

  let single_steps = (steps: EStep.Set.t) =>
    steps
    |> List.map(step => (step, []))
    |> EStep.Map.of_list;

  let union = EStep.Map.union((_, l, r) => ListUtil.dedup(l @ r));
  let union_all = List.fold_left(union, empty);

  let add = (w, set) =>
    switch (split_dst(w)) {
    | None => set
    | Some((dst, w)) =>
      EStep.Map.update(
        dst,
        fun
        | None => [w]
        | Some(ws) => ListUtil.dedup([w, ...ws]),
      );
    };

  let single = w =>
    switch (split_dst(w)) {
    | None => EStep.Map.empty
    | Some((dst, w)) => EStep.Map.singleton(dst, [w]);
    };

  // let map = (f: Base.t => Base.t, set) =>
  //   EStep.Map.to_list(set)
  //   |> List.map(((dst, ws)) => (dst, List.map(f, ws)))
  //   |> EStep.Map.of_list;

  let bind = (set, f: Base.t => t) => {
    open ListUtil.Syntax;
    let* (dst, ws) = EStep.Map.to_list(set);
    let+ w = ws;
    f(cons(dst, w));
  }
  |> union_all;

  module Syntax = {
    let return = single;
    // let (let+) = Fun.flip(map);
    let (let*) = bind;
    // let (let^) = (set, f) => {
    //   let* (dst, w) = set;
    //   f((dst, w)) |> add(dst, w);
    // };
  };
};

let step = (d: Dir.t, step: EStep.t): EStep.Set.t =>
  switch (step) {
  | Mold(Space) => failwith("todo");
  };

let enter = (from: Dir.t, s: Bound.t(ESort.t)): EStep.Set.t =>
  switch (s) {
  | Root =>
    let tiles = Set.of_g(GWalk.enter(~from, Sort.root));
    let grout = EStep.Set.entry_grout(from, Sort.root);
    let _ = failwith("todo: figure out how space works here");
    Set.union(tiles, grout);
  | Node(Tile(gs)) =>
    let tiles = Set.of_g(GWalk.enter_g(~from, gs));
    let grout = EStep.Set.entry_grout(from, GSort.sort(s));
    let _ = failwith("todo: figure out how space works here");
    Set.union(tiles, grout);
  | Node(Space) => Set.singleton(singleton(EStep.Mold(Space)))
  | Node(Grout({l, s, r})) =>
    let _ = failwith("todo: enter grout")
    let (ls, rs) = GWalk.(sort_deps(L, s), sort_deps(R, s));
    (l ? ls : []) @ (r ? rs : [])
    |> ListUtil.dedup
    |> List.map(GWalk.enter(~from))
    |> GWalk.Set.union_all
    |> Set.of_g;
  };

let walk = (d: Dir.t, src: Bound.t(EStep.t)): Set.t => {
  let seen = Hashtbl.create(100);
  let rec go = (~src, walked) => {
    let go = go(~src);
    let s = Option.value(dst(walked), ~default=src);
    switch (Hashtbl.find_opt(seen, s)) {
    | Some() => walked
    | None =>
      Hashtbl.add(seen, s, ());
      switch (s) {
      | Mold(_) =>
        let* stepped = Set.of_steps(step(d, s));
        go(cat_eq(stepped, walked));
      | Meld(sort) =>
        let entered = {
          let* entered = Set.of_steps(enter(Dir.toggle(d), Node(sort)));
          go(cat_neq(entered, walked));
        };
        let stepped = {
          let* stepped = Set.of_steps(step(d, s));
          go(cat_neq(stepped, walked));
        };
        Set.union(stepped, entered);
      };
    };
  };
  switch (src) {
  | Node(src) => go(~src, empty)
  | Root =>
    let* entered = Set.of_steps(enter(Dir.toggle(d), Root));
    go(~src=dst(entered), mk([[], []]));
  };
};
