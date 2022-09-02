open Sexplib.Std;
open Util;

module ShardInfo = {
  module Order = {
    type key = (Id.t, int);
    // let key = (s: Shard.t) => (s.tile_id, Shard.index(s));

    open Hashtbl;
    type t = Hashtbl.t(key, Hashtbl.t(key, unit));

    let n = 20;
    let init = () => create(n);

    let lt = (l: Tile.t, r: Tile.t, ord: t): bool => {
      let (i_l, i_r) = Tile.(r_shard(l), l_shard(r));
      switch (find_opt(ord, (l.id, i_l))) {
      | None => false
      | Some(row) => Option.is_some(find_opt(row, (r.id, i_r)))
      };
    };
    let gt = (l, r, ord) => lt(r, l, ord);
    let un = (l, r, ord) => !lt(l, r, ord) && !gt(l, r, ord);

    let disordered = (t: Tile.t, t': Tile.t): bool =>
      t.id == t'.id
      && {
        let (l, r) = Tile.(l_shard(t), r_shard(t));
        let (l', r') = Tile.(l_shard(t'), r_shard(t'));
        l < l' && l' < r || l' < l && l < r';
      };

    let lt_or_un = (ls, rs, ord) =>
      ls
      |> List.for_all(l =>
           rs
           |> List.for_all(r =>
                !disordered(l, r) && (lt(l, r, ord) || un(l, r, ord))
              )
         );

    let get = (i, j, m) => {
      open OptUtil.Syntax;
      let* r = find_opt(m, i);
      find_opt(r, j);
    };

    let set = (i, j, m) => {
      let r =
        switch (find_opt(m, i)) {
        | None => create(n)
        | Some(r) => r
        };
      replace(r, j, ());
      replace(m, i, r);
    };

    let add_tile = (id, lbl, ord) =>
      lbl
      |> List.iteri((i, _) => {
           switch (find_opt(ord, (id, i))) {
           | Some(_) => ()
           | None => replace(ord, (id, i), create(n))
           };
           i == 0 ? () : set((id, i - 1), (id, i), ord);
         });

    // Warshall's algorithm https://cs.winona.edu/lin/cs440/ch08-2.pdf
    let tran_close = (ord: t): unit => {
      let keys = List.of_seq(Hashtbl.to_seq_keys(ord));
      keys
      |> List.iteri((n, k) =>
           if (n == 0) {
             ();
           } else {
             keys
             |> List.iter(i =>
                  keys
                  |> List.iter(j =>
                       switch (get(i, j, ord)) {
                       | Some(_) => ()
                       | None =>
                         switch (get(i, k, ord), get(k, j, ord)) {
                         | (None, _)
                         | (_, None) => ()
                         | (Some(_), Some(_)) => set(i, j, ord)
                         }
                       }
                     )
                );
           }
         );
    };
  };

  module Count = {
    type t = {
      labels: Id.Map.t(Label.t),
      counts: Id.Map.t(int),
    };

    let of_tile = (t: Tile.t) => {
      labels: Id.Map.singleton(t.id, t.label),
      counts: Id.Map.singleton(t.id, List.length(t.shards)),
    };

    let merge = (m: t, m': t) => {
      labels: Id.Map.union((_, lbl, _) => Some(lbl), m.labels, m'.labels),
      counts: Id.Map.union((_, n, n') => Some(n + n'), m.counts, m'.counts),
    };

    let mem = (id, m) => Id.Map.mem(id, m.labels);

    let exists_mem = (ts: list(Tile.t), m) =>
      List.exists((t: Tile.t) => mem(t.id, m), ts);

    let is_complete = (m: t) =>
      m.counts
      |> Id.Map.for_all((id, n) =>
           n == List.length(Id.Map.find(id, m.labels))
         );
  };

  module Counts = {
    type t = Id.Uf.store(Count.t);
    include Id.Uf;
    let merge = merge(Count.merge);
    let add_tile = (t: Tile.t, cs: t): unit =>
      switch (get_opt(t.id, cs)) {
      | None => add(t.id, Count.of_tile(t), cs)
      | Some(c) =>
        let c = {
          ...c,
          counts:
            Id.Map.update(
              t.id,
              Option.map((+)(List.length(t.shards))),
              c.counts,
            ),
        };
        set(t.id, c, cs);
      };
  };

  // Shards are considered selection-matching if they are related transitively
  // by normal within-tile matching or by same-selection-containment.
  // Selection-matching shards are ordered transitively by within-tile
  // ordering and same-selection ordering combined with well-nestedness.
  // Represents the expected order of all selection-matching shards as
  // imposed by selections in the backpack, along with counts of those
  // shards contained in the backpack.
  // Counts are partitioned by the selection-matching relation.
  type t = {
    order: Order.t,
    counts: Counts.t,
  };

  let init = () => {order: Order.init(), counts: Counts.init()};

  let add_sel = (sel: Selection.t, {counts, order}: t): unit => {
    let ts = Segment.incomplete_tiles(sel.content);
    // initialize
    ts
    |> List.iter((t: Tile.t) => {
         Counts.add_tile(t, counts);
         Order.add_tile(t.id, t.label, order);
       });
    // merge counts
    ignore(
      ts
      |> List.fold_left(
           (prev: option(Tile.t), curr: Tile.t) => {
             switch (prev) {
             | None => ()
             | Some(prev) => Counts.merge(prev.id, curr.id, counts)
             };
             Some(curr);
           },
           None,
         ),
    );
    // propagate well-nested ordering constraints
    ListUtil.ordered_pairs(ts)
    |> List.iter(((l: Tile.t, r: Tile.t)) => {
         let (n_l, n_r) = List.(length(l.label), length(r.label));
         let (i_l, i_r) = Tile.(r_shard(l), l_shard(r));
         Order.set((l.id, i_l), (r.id, i_r), order);
         if (i_l == n_l - 1 && i_r != 0) {
           // l must be nested within r
           Order.set(
             (r.id, i_r - 1),
             (l.id, 0),
             order,
           );
         } else if (i_l != n_l - 1 && i_r == 0) {
           // r must be nested within l
           Order.set(
             (r.id, n_r - 1),
             (l.id, i_l + 1),
             order,
           );
         };
       });
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(Selection.t);

let empty = [];

let shard_info = (bp: t) => {
  open ShardInfo;
  let info = init();
  bp |> List.iter(sel => add_sel(sel, info));
  Order.tran_close(info.order);
  info;
};

let push = sel => Selection.is_empty(sel) ? Fun.id : List.cons(sel);

let push_s: (list(Selection.t), t) => t = List.fold_right(push);

let pop =
    ((pre, suf): (list(Tile.t), list(Tile.t)), bp: t)
    : option((bool, Selection.t, t)) => {
  open OptUtil.Syntax;
  let* (hd, tl) = ListUtil.split_first_opt(bp);
  switch (Segment.incomplete_tiles(hd.content)) {
  | [] => Some((true, hd, tl))
  | [t, ..._] as ts =>
    open ShardInfo;
    let {counts, order} = shard_info(bp);
    let count = Counts.get(t.id, counts);
    let first = Count.is_complete(count);
    first
    || (Count.exists_mem(pre, count) || Count.exists_mem(suf, count))
    && Order.lt_or_un(pre, ts, order)
    && Order.lt_or_un(ts, suf, order)
      ? Some((first, hd, tl)) : None;
  };
};

let restricted = (bp: t): bool =>
  switch (bp) {
  | [] => false
  | [hd, ..._] =>
    switch (Segment.incomplete_tiles(hd.content)) {
    | [] => false
    | [t, ..._] =>
      open ShardInfo;
      let info = shard_info(bp);
      !Count.is_complete(Counts.get(t.id, info.counts));
    }
  };

let remove_matching = (ts: list(Tile.t), bp: t) =>
  List.fold_left(
    (bp, t: Tile.t) =>
      bp
      |> List.map(Selection.map(Segment.remove_matching(t)))
      |> List.filter_map(
           fun
           | sel when !Selection.is_empty(sel) => Some(sel)
           | _ => None,
         ),
    bp,
    ts,
  );

let is_first_matching = (t: Token.t, bp: t): bool =>
  /* Does the first selection in the backpack consist
     of a single token which matches the one provided? */
  switch (bp) {
  | [] => false
  | [{content: [p], _}, ..._] =>
    switch (p) {
    | Tile({shards: [i], label, _}) =>
      assert(i < List.length(label));
      List.nth(label, i) == t;
    | _ => false
    }
  | _ => false
  };
