open Util;

exception Empty_segment;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Base.segment;

let empty = [];
let cons = List.cons;
let concat = List.concat;
let fold_right = List.fold_right;
let rev = List.rev;

let of_tile = t => [Tile.to_piece(t)];

let nibs = tiles =>
  switch (tiles, ListUtil.split_last_opt(tiles)) {
  | ([], _)
  | (_, None) => None
  | ([_first, ..._], Some((_, _last))) => failwith("todo Tiles.nibs")
  };

let incomplete_tiles =
  List.filter_map(
    fun
    | Piece.Tile(t) when !Tile.is_complete(t) => Some(t)
    | _ => None,
  );
let tiles =
  List.filter_map(
    fun
    | Piece.Tile(t) => Some(t)
    | _ => None,
  );

let convex_grout =
  List.filter_map(
    fun
    | Piece.Grout(g) when g.shape == Convex => Some(g)
    | _ => None,
  );

let contains_matching = (t: Tile.t) =>
  List.exists(
    fun
    | Piece.Tile(t') => t'.id == t.id
    | _ => false,
  );

let remove_matching = (t: Tile.t) =>
  List.filter_map(
    fun
    | Piece.Tile(t') when t'.id == t.id => None
    | p => Some(p),
  );

let snoc = (tiles, tile) => tiles @ [tile];

// let is_balanced = List.for_all(Piece.is_balanced);

let shape_affix =
    (d: Direction.t, affix: t, r: Nib.Shape.t)
    : (Aba.t(list(Whitespace.t), Grout.t), Nib.Shape.t, t) => {
  let empty_wgw = Aba.mk([[]], []);
  let rec go = (affix: t, r: Nib.Shape.t) =>
    switch (affix) {
    | [] => (empty_wgw, r, [])
    | [p, ...tl] =>
      let (wgw, s, tl) = go(tl, r);
      switch (p) {
      | Whitespace(w) =>
        let (wss, gs) = wgw;
        let (ws, wss) = ListUtil.split_first(wss);
        (([[w, ...ws], ...wss], gs), s, tl);
      | Grout(g) => (Aba.cons([], g, wgw), s, tl)
      | Tile(t) =>
        let (l, _) = Tile.shapes(t) |> (d == Left ? TupleUtil.swap : Fun.id);
        (empty_wgw, l, tl);
      };
    };
  go((d == Left ? List.rev : Fun.id)(affix), r);
};
let shape = shape_affix(Right);

let rec convex = seg => {
  open OptUtil.Syntax;
  let l =
    fold_right(
      (p: Piece.t, shape) => {
        let* s = shape;
        switch (p) {
        | Whitespace(_) => shape
        | Grout(g) =>
          Grout.fits_shape(g, s) ? Some(fst(Grout.shapes(g))) : None
        | Tile(t) =>
          let (l, r) = Tile.shapes(t);
          List.for_all(convex, t.children) && Nib.Shape.fits(r, s)
            ? Some(l) : None;
        };
      },
      seg,
      Some(Nib.Shape.concave()),
    );
  switch (l) {
  | None => false
  | Some(l) => Nib.Shape.fits(Nib.Shape.concave(), l)
  };
};

let split_by_grout: t => Aba.t(t, Grout.t) =
  Aba.split(
    fun
    | Piece.Grout(g) => Either.R(g)
    | p => L(p),
  );

// remold top-level mold of tile according to given sort and shape
let remold_tile_shallow =
    (~sort: Sort.t, ~shape: Nib.Shape.t, t: Tile.t): option(Tile.t) =>
  Molds.get(t.label)
  |> List.filter_map((mold: Mold.t) =>
       mold.out == sort ? Some({...t, mold}) : None
     )
  |> (
    fun
    | [_] as ts => ts
    | ts =>
      ts |> List.filter(t => Nib.Shape.fits(shape, fst(Tile.shapes(t))))
  )
  |> ListUtil.hd_opt;

// attempt to remold tile as sorts in the stack with concave left nib
let rec remold_tile_via_stack =
        (~stack: list(Sort.t), t: Tile.t): option((list(Sort.t), Tile.t)) =>
  switch (stack) {
  | [] => None
  | [sort, ...stack] =>
    // Convex to get parent
    switch (remold_tile_shallow(~sort, ~shape=Nib.Shape.Convex, t)) {
    | Some(t) when fst(Tile.shapes(t)) != Convex => Some((stack, t))
    | _ => remold_tile_via_stack(~stack, t)
    }
  };

module Greg = {
  type seg = t;
  type t = Aba.t(Grout.t, Tile.t);

  let empty = ([Grout.empty], []);

  let cons_w = (w: Whitespace.t) => Aba.update_first_a(Grout.cons_w(w));
  let cons_h = (h: Hole.t) => Aba.update_first_a(Grout.cons_h(h));
  let cons_t = (t: Tile.t) => Aba.cons(Grout.empty, t);

  let cons = (p: Piece.t) =>
    switch (p) {
    | Whitespace(w) => cons_w(w)
    | Grout(h) => cons_h(h)
    | Tile(t) => cons_t(t)
    };

  let of_seg = (seg: seg) => List.fold_right(cons, seg, empty);

  let rec remold =
          (
            ~stack: list(Sort.t)=[],
            ~sort: Sort.t,
            ~shape=Nib.Shape.concave(),
          )
          : (t => t) =>
    failwith("todo");
};

let rec remold =
        (
          ~stack: list(Sort.t)=[],
          ~sort: Sort.t,
          ~shape=Nib.Shape.concave(),
          seg: t,
        )
        : t =>
  switch (seg) {
  | [] => []
  | [hd, ...tl] =>
    switch (hd) {
    | Whitespace(_)
    | Grout(_) =>
      // failwith("todo: add trim to remold accumulator")
      [hd, ...remold(~stack, ~sort, ~shape, tl)]
    | Tile(t) =>
      let remold_rest = (~stack, t: Tile.t): t => {
        // if convex shape, s should be same as sort
        let (_, Nib.{sort: s, shape}) = Tile.nibs(t);
        let stack = s != sort ? [sort, ...stack] : stack;
        [Tile(t), ...remold(~stack, ~sort=s, ~shape, tl)];
      };
      switch (remold_tile_via_stack(~stack, t)) {
      | Some((stack, t)) => remold_rest(~stack, t)
      | None =>
        switch (remold_tile(~sort, ~shape, t)) {
        | Some(t) => remold_rest(~stack, t)
        | None =>
          switch (stack) {
          | [] => remold_rest(~stack, t)
          | [sort, ...stack] =>
            // pop stack and try again
            remold(~stack, ~sort, ~shape, seg)
          }
        }
      };
    }
  }
and remold_tile =
    (~sort: Sort.t, ~shape: Nib.Shape.t, t: Tile.t): option(Tile.t) => {
  open OptUtil.Syntax;
  let+ remolded = remold_tile_shallow(~sort, ~shape, t);
  Effect.s_remold(t.id, t.mold, remolded.mold);
  let children =
    Aba.aba_triples(Aba.mk(remolded.shards, remolded.children))
    |> List.map(((l, child, r)) => {
         let lr_neighbors = l + 1 == r;
         let sort_changed =
           List.nth(remolded.mold.in_, l) != List.nth(t.mold.in_, l);
         let just_touched = Effect.s_touched(remolded.id);
         lr_neighbors && (sort_changed || just_touched)
           ? remold(~sort=List.nth(remolded.mold.in_, l).sort, child) : child;
       });
  {...remolded, children};
};

let skel = seg =>
  seg
  |> List.mapi((i, p) => (i, p))
  |> List.filter(((_, p)) => !Piece.is_whitespace(p))
  |> Skel.mk;

let nibbed_children = List.concat_map(Piece.nibbed_children);
let children = seg => List.map(snd, nibbed_children(seg));

let of_grout = (g: Grout.t): t =>
  g
  |> Aba.join(List.map(Piece.whitespace), h => [Piece.Grout(h)])
  |> List.concat;

let rec regrout = ((l, r): Nibs.t, seg, s: Sort.t) => {
  open IdGen.Syntax;
  let* (trim, r, tl) = regrout_affix(Direction.Right, seg, r, s);
  let+ (_, trim) = Grout.regrout((l, r), trim, s);
  of_grout(trim) @ tl;
}
and regrout_affix =
    (d: Direction.t, affix: t, r: Nib.t, s: Sort.t)
    : IdGen.t((Grout.t, Nib.t, t)) => {
  open IdGen.Syntax;
  let+ (trim, s, affix) =
    fold_right(
      (p: Piece.t, id_gen) => {
        let* (trim, r, tl) = id_gen;
        switch (p) {
        | Whitespace(w) => IdGen.return((Grout.cons_w(w, trim), r, tl))
        | Grout(g) => IdGen.return((Grout.cons_g(g, trim), r, tl))
        | Tile(t) =>
          let* children =
            Effect.s_touched(t.id)
            || Option.is_some(Effect.s_remolded(t.id))
              ? List.fold_right(
                  (((l, r) as nibs: Nibs.t, hd), tl) => {
                    let* tl = tl;
                    let s = l.sort == r.sort ? l.sort : Sort.Any;
                    let+ hd = regrout(nibs, hd, s);
                    [hd, ...tl];
                  },
                  Tile.nibbed_children(t),
                  IdGen.return([]),
                )
              : return(t.children);
          let p = Piece.Tile({...t, children});
          let (l', r') =
            Tile.nibs(t) |> (d == Left ? TupleUtil.swap : Fun.id);
          let+ (_, trim) = Grout.regrout((r', r), trim, s);
          (Grout.empty, l', [p, ...Grout.to_seg(trim)] @ tl);
        };
      },
      (d == Left ? List.rev : Fun.id)(affix),
      IdGen.return((Aba.mk([[]], []), r, empty)),
    );
  d == Left ? (Grout.rev(trim), s, rev(affix)) : (trim, s, affix);
};

let split_by_matching = (id: Id.t): (t => Aba.t(t, Tile.t)) =>
  Aba.split(
    fun
    | Piece.Tile(t) when t.id == id => Either.R(t)
    | p => L(p),
  );

let matches = (l: Tile.t, r: Tile.t): bool =>
  l.label == r.label && Tile.r_shard(l) + 1 == Tile.l_shard(r);

module Split = {
  type seg = t;
  // invariant: segs consist of tiles with both ends
  // invariant: tiles are missing an end
  type t = Aba.t(seg, Tile.t);

  let empty = Aba.mk([empty], []);

  let flatten = split =>
    split |> Aba.join(Fun.id, t => [Tile.to_piece(t)]) |> List.flatten;

  let cons_complete = (p, split) => Aba.update_first_a(cons(p), split);
  let cons_incomplete = (t, split) => Aba.cons(empty, t, split);

  let push = (p: Piece.t, split: t) => {
    switch (p) {
    | Tile(t) when !Tile.has_end(Right, t) =>
      let cons = t =>
        Tile.has_end(Left, t)
        ? cons_complete(Piece.Tile(t))
        : cons_incomplete(t);
      let rec match = (split: t) =>
        switch (Aba.uncons(split)) {
        | None => cons(t, split)
        | Some((seg, t', split')) =>
          switch (assemble(t, seg, t')) {
          | None => Aba.cons(seg, t', match(split'))
          | Some(t) => cons(t, split')
          }
        };
      match(split);
    | Tile(t) when !Tile.has_end(Left, t) =>
      cons_incomplete(t, split)
    | _ => cons_complete(p, split)
    };
};

let reassemble = (seg: t): t =>
  List.fold_right(Split.push, seg, ([empty], []))
  |> Split.flatten;

let trim_f: (list(Base.piece) => list(Base.piece), Direction.t, t) => t =
  (trim_l, d, ps) => {
    switch (d) {
    | Left => ps |> trim_l
    | Right => ps |> List.rev |> trim_l |> List.rev
    };
  };

let trim_whitespace: (Direction.t, t) => t =
  (d, ps) => {
    /* Trims leading/trailing whitespace */
    let rec trim_l = xs =>
      switch (xs) {
      | [] => []
      | [Piece.Whitespace(_), ...xs] => trim_l(xs)
      | [_, ..._] => xs
      };
    trim_f(trim_l, d, ps);
  };

let trim_whitespace_and_grout: (Direction.t, t) => t =
  (d, ps) => {
    /* Trims leading/trailing whitespace, continuing
       to trim around grout until first Tile is reached */
    let rec trim_l: list(Base.piece) => list(Base.piece) =
      xs =>
        switch (xs) {
        | [] => []
        | [Whitespace(_) | Grout(_), ...xs] => trim_l(xs)
        | [_, ..._] => xs
        };
    trim_f(trim_l, d, ps);
  };

let trim_grout_around_whitespace: (Direction.t, t) => t =
  (d, ps) => {
    /* Trims leading/trailing grout, skipping over whitespace,
       but not skipping over other pieces. */
    let rec trim_l: list(Base.piece) => list(Base.piece) =
      xs =>
        switch (xs) {
        | [] => []
        | [Whitespace(w), ...xs] => [Whitespace(w), ...trim_l(xs)]
        | [Grout(_), ...xs] => trim_l(xs)
        | [_, ..._] => xs
        };
    trim_f(trim_l, d, ps);
  };

let edge_nib_of = (d: Direction.t, ps: t): option(Nib.t) => {
  let trimmed = trim_whitespace(d, ps);
  switch (d, ListUtil.hd_opt(trimmed), ListUtil.last_opt(trimmed)) {
  | (Right, _, Some(p)) => p |> Piece.nibs |> Option.map(snd)
  | (Left, Some(p), _) => p |> Piece.nibs |> Option.map(fst)
  | _ => None
  };
};
let edge_shape_of = (d, ps): option(Nib.Shape.t) =>
  edge_nib_of(d, ps) |> Option.map((nib: Nib.t) => nib.shape);

let edge_direction_of = (d: Direction.t, ps: t): option(Direction.t) =>
  Option.map(Nib.Shape.absolute(d), edge_shape_of(d, ps));

let rec serialize = (seg: t) =>
  seg
  |> List.concat_map(
       fun
       | (Piece.Whitespace(_) | Grout(_) | Tile({shards: [_], _})) as p => [
           p,
         ]
       | Tile(t) => {
           let shards =
             List.map(
               Tile.to_piece,
               Tile.split_shards(t.id, t.label, t.mold, t.shards),
             );
           let children = List.map(serialize, t.children);
           Aba.mk(shards, children)
           |> Aba.join(s => [s], Fun.id)
           |> List.concat;
         },
     );

let sameline_whitespace =
  List.for_all(
    fun
    | Piece.Whitespace(w) => w.content != Whitespace.linebreak
    | _ => false,
  );

let expected_sorts = (sort: Sort.t, seg: t): list((int, Sort.t)) => {
  let p = List.nth(seg);
  let rec go = (sort: Sort.t, skel: Skel.t): list((list(int), Sort.t)) => {
    let root = Skel.root(skel);
    let inside_sorts =
      Aba.aba_triples(root)
      |> List.concat_map(((n_l, kid, n_r)) => {
           let (_, s_l) = Piece.nib_sorts(p(n_l));
           let (s_r, _) = Piece.nib_sorts(p(n_r));
           let s = s_l == s_r ? s_l : Sort.Any;
           go(s, kid);
         });
    let outside_sorts = {
      let ns = Aba.get_as(root);
      let (l_sort, _) = Piece.nib_sorts(p(Aba.first_a(root)));
      let (_, r_sort) = Piece.nib_sorts(p(Aba.last_a(root)));
      switch (skel) {
      | Op(_) => [(ns, sort)]
      | Pre(_, r) => [(ns, sort)] @ go(r_sort, r)
      | Post(l, _) => go(l_sort, l) @ [(ns, sort)]
      | Bin(l, _, r) => go(l_sort, l) @ [(ns, sort)] @ go(r_sort, r)
      };
    };
    outside_sorts @ inside_sorts;
  };
  go(sort, skel(seg))
  |> List.concat_map(((ns, s)) => List.map(n => (n, s), ns));
};

let hard_nib = (d: Direction.t, seg: t): option(Nib.t) => {
  let rec go =
    fun
    | [] => None
    | [Piece.Tile(t), ..._] => Some(Direction.choose(d, Tile.nibs(t)))
    | [Grout(_) | Whitespace(_), ...tl] => go(tl);
  go(d == Left ? seg : rev(seg));
};
