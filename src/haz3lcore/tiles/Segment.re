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
    : (Aba.t(list(Secondary.t), Grout.t), Nib.Shape.t, t) => {
  let empty_wgw = Aba.mk([[]], []);
  let rec go = (affix: t, r: Nib.Shape.t) =>
    switch (affix) {
    | [] => (empty_wgw, r, [])
    | [p, ...tl] =>
      let (wgw, s, tl) = go(tl, r);
      switch (p) {
      | Livelit(_) => (empty_wgw, r, []) // TODO Livelit. I have no idea what this method does.
      | Secondary(w) =>
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
        | Livelit(_) // TODO Livelit
        | Secondary(_) => shape
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

let rec remold = (~shape=Nib.Shape.concave(), seg: t, s: Sort.t) =>
  switch (s) {
  | Any => seg
  | Typ => remold_typ(shape, seg)
  | Pat => remold_pat(shape, seg)
  | Exp => remold_exp(shape, seg)
  | Rul => remold_rul(shape, seg)
  | _ => failwith("remold unexpected")
  }
and remold_tile = (s: Sort.t, shape, t: Tile.t): option(Tile.t) => {
  open OptUtil.Syntax;
  let+ remolded =
    Molds.get(t.label)
    |> List.filter((m: Mold.t) => m.out == s)
    |> List.map(mold => {...t, mold})
    |> (
      fun
      | [_] as ts => ts
      | ts =>
        ts |> List.filter(t => Nib.Shape.fits(shape, fst(Tile.shapes(t))))
    )
    |> ListUtil.hd_opt;
  let children =
    List.fold_right(
      ((l, child, r), children) => {
        let child =
          if (l
              + 1 == r
              && (
                List.nth(remolded.mold.in_, l) != List.nth(t.mold.in_, l)
                || Effect.s_touched(remolded.id)
              )) {
            remold(child, List.nth(remolded.mold.in_, l));
          } else {
            child;
          };
        [child, ...children];
      },
      Aba.aba_triples(Aba.mk(remolded.shards, remolded.children)),
      [],
    );
  {...remolded, children};
}
and remold_typ = (shape: Nib.Shape.t, seg: t): t =>
  switch (seg) {
  | [] => []
  | [hd, ...tl] =>
    switch (hd) {
    | Livelit(_) // TODO Livelit
    | Secondary(_)
    | Grout(_) => [hd, ...remold_typ(shape, tl)]
    | Tile(t) =>
      switch (remold_tile(Typ, shape, t)) {
      | None => [Tile(t), ...remold_typ(snd(Tile.shapes(t)), tl)]
      | Some(t) => [Tile(t), ...remold_typ(snd(Tile.shapes(t)), tl)]
      }
    }
  }
and remold_typ_uni = (shape, seg: t): (t, Nib.Shape.t, t) =>
  switch (seg) {
  | [] => ([], shape, [])
  | [hd, ...tl] =>
    switch (hd) {
    | Livelit(_) // TODO Livelit
    | Secondary(_)
    | Grout(_) =>
      let (remolded, shape, rest) = remold_typ_uni(shape, tl);
      ([hd, ...remolded], shape, rest);
    | Tile(t) =>
      switch (remold_tile(Typ, shape, t)) {
      | None => ([], shape, seg)
      | Some(t) when !Tile.has_end(Right, t) =>
        let (_, r) = Tile.nibs(t);
        let remolded = remold(~shape=r.shape, tl, r.sort);
        let (_, shape, _) = shape_affix(Left, remolded, r.shape);
        ([Tile(t), ...remolded], shape, []);
      | Some(t) when t.label == Form.get("comma_typ").label => (
          [],
          shape,
          seg,
        )
      | Some(t) =>
        let (remolded, shape, rest) =
          remold_typ_uni(snd(Tile.shapes(t)), tl);
        ([Tile(t), ...remolded], shape, rest);
      }
    }
  }
and remold_pat_uni = (shape, seg: t): (t, Nib.Shape.t, t) =>
  switch (seg) {
  | [] => ([], shape, [])
  | [hd, ...tl] =>
    switch (hd) {
    | Livelit(_) // TODO Livelit
    | Secondary(_)
    | Grout(_) =>
      let (remolded, shape, rest) = remold_pat_uni(shape, tl);
      ([hd, ...remolded], shape, rest);
    | Tile(t) =>
      switch (remold_tile(Pat, shape, t)) {
      | None => ([], shape, seg)
      | Some(t) when !Tile.has_end(Right, t) =>
        let (_, r) = Tile.nibs(t);
        let remolded = remold(~shape=r.shape, tl, r.sort);
        let (_, shape, _) = shape_affix(Left, remolded, r.shape);
        ([Tile(t), ...remolded], shape, []);
      | Some(t) =>
        switch (Tile.nibs(t)) {
        | (_, {shape, sort: Typ}) =>
          let (remolded_typ, shape, rest) = remold_typ_uni(shape, tl);
          let (remolded_pat, shape, rest) = remold_pat_uni(shape, rest);
          ([Piece.Tile(t), ...remolded_typ] @ remolded_pat, shape, rest);
        | _ =>
          let (remolded, shape, rest) =
            remold_pat_uni(snd(Tile.shapes(t)), tl);
          ([Tile(t), ...remolded], shape, rest);
        }
      }
    }
  }
and remold_pat = (shape, seg: t): t =>
  switch (seg) {
  | [] => []
  | [hd, ...tl] =>
    switch (hd) {
    | Livelit(_) // TODO Livelit
    | Secondary(_)
    | Grout(_) => [hd, ...remold_pat(shape, tl)]
    | Tile(t) =>
      switch (remold_tile(Pat, shape, t)) {
      | None => [Tile(t), ...remold_pat(snd(Tile.shapes(t)), tl)]
      | Some(t) =>
        switch (Tile.nibs(t)) {
        | (_, {shape, sort: Typ}) =>
          let (remolded, shape, rest) = remold_typ_uni(shape, tl);
          [Piece.Tile(t), ...remolded] @ remold_pat(shape, rest);
        | _ => [Tile(t), ...remold_pat(snd(Tile.shapes(t)), tl)]
        }
      }
    }
  }
and remold_exp_uni = (shape, seg: t): (t, Nib.Shape.t, t) =>
  switch (seg) {
  | [] => ([], shape, [])
  | [hd, ...tl] =>
    switch (hd) {
    | Livelit(_) // TODO Livelit
    | Secondary(_)
    | Grout(_) =>
      let (remolded, shape, rest) = remold_exp_uni(shape, tl);
      ([hd, ...remolded], shape, rest);
    | Tile(t) =>
      switch (remold_tile(Exp, shape, t)) {
      | None => ([], shape, seg)
      | Some(t) when !Tile.has_end(Right, t) =>
        let (_, r) = Tile.nibs(t);
        let remolded = remold(~shape=r.shape, tl, r.sort);
        let (_, shape, _) = shape_affix(Left, remolded, r.shape);
        ([Tile(t), ...remolded], shape, []);
      | Some(t) =>
        switch (Tile.nibs(t)) {
        | (_, {shape, sort: Pat}) =>
          let (remolded_pat, shape, rest) = remold_pat_uni(shape, tl);
          let (remolded_exp, shape, rest) = remold_exp_uni(shape, rest);
          ([Piece.Tile(t), ...remolded_pat] @ remolded_exp, shape, rest);
        | (_, {shape, sort: Rul}) =>
          // TODO review short circuit
          ([Tile(t)], shape, tl)
        | _ =>
          let (remolded, shape, rest) =
            remold_exp_uni(snd(Tile.shapes(t)), tl);
          ([Tile(t), ...remolded], shape, rest);
        }
      }
    }
  }
and remold_rul = (shape, seg: t): t =>
  switch (seg) {
  | [] => []
  | [hd, ...tl] =>
    switch (hd) {
    | Livelit(_) // TODO Livelit
    | Secondary(_)
    | Grout(_) => [hd, ...remold_rul(shape, tl)]
    | Tile(t) =>
      switch (remold_tile(Rul, shape, t)) {
      | Some(t) =>
        switch (Tile.nibs(t)) {
        | (_, {shape, sort: Exp}) =>
          let (remolded, shape, rest) = remold_exp_uni(shape, tl);
          [Piece.Tile(t), ...remolded] @ remold_rul(shape, rest);
        | (_, {shape, sort: Pat}) =>
          let (remolded, shape, rest) = remold_pat_uni(shape, tl);
          // TODO(d) continuing onto rule might not be right right...
          [Piece.Tile(t), ...remolded] @ remold_rul(shape, rest);
        | _ => failwith("remold_rul unexpected")
        }
      | None =>
        let (remolded, shape, rest) = remold_exp_uni(shape, [hd, ...tl]);
        switch (remolded) {
        | [] => [Piece.Tile(t), ...remold_rul(shape, tl)]
        | [_, ..._] => remolded @ remold_rul(shape, rest)
        };
      }
    }
  }
and remold_exp = (shape, seg: t): t =>
  switch (seg) {
  | [] => []
  | [hd, ...tl] =>
    switch (hd) {
    | Livelit(_) // TODO Livelit
    | Secondary(_)
    | Grout(_) => [hd, ...remold_exp(shape, tl)]
    | Tile(t) =>
      switch (remold_tile(Exp, shape, t)) {
      | None => [Tile(t), ...remold_exp(snd(Tile.shapes(t)), tl)]
      | Some(t) =>
        switch (Tile.nibs(t)) {
        | (_, {shape, sort: Pat}) =>
          let (remolded, shape, rest) = remold_pat_uni(shape, tl);
          [Piece.Tile(t), ...remolded] @ remold_exp(shape, rest);
        | (_, {shape, sort: Rul}) => [Tile(t), ...remold_rul(shape, tl)]
        | _ => [Tile(t), ...remold_exp(snd(Tile.shapes(t)), tl)]
        }
      }
    }
  };

let skel = seg =>
  seg
  |> List.mapi((i, p) => (i, p))
  |> List.filter(((_, p)) => !Piece.is_secondary(p))
  |> Skel.mk;

let sorted_children = List.concat_map(Piece.sorted_children);
let children = seg => List.map(snd, sorted_children(seg));

module Trim = {
  type seg = t;
  type t = Aba.t(list(Secondary.t), Grout.t);

  let empty = Aba.mk([[]], []);

  let rev = Aba.rev(List.rev, Fun.id);

  let cons_w =
      (w: Secondary.t, (wss, gs): Aba.t(list(Secondary.t), Grout.t)) => {
    // safe bc Aba always has at least one A element
    let (ws, wss) = ListUtil.split_first(wss);
    Aba.mk([[w, ...ws], ...wss], gs);
  };
  let cons_g = (g: Grout.t, (wss, gs)): Aba.t(list(Secondary.t), Grout.t) =>
    Aba.mk([[], ...wss], [g, ...gs]);

  let ws = ((wss, _): t): seg => List.(map(Piece.secondary, concat(wss)));

  // postcond: result is either <ws> or <ws,g,ws'>
  let merge = ((wss, gs): t): t => {
    switch (Grout.merge(gs)) {
    | None => Aba.mk([List.concat(wss)], [])
    | Some(g) =>
      let (ws, wss) = ListUtil.split_first(wss);
      Aba.mk([ws, List.concat(wss)], [g]);
    };
  };
  // same as merge but type encodes postcond
  // let merged = (trim: t): (list(Secondary.t), option((Grout.t, list(Secondary.t)))) => {
  //   let (wss, gs) = merge(trim);
  //   let (ws, wss) = ListUtil.split_first(wss);
  //   switch (gs) {
  //   | [] => (ws, None)
  //   | [g, ..._] => (ws, Some((g, List.concat(wss))))
  //   };
  // };
  let rec rm_up_to_one_space =
          (wss: list(list(Secondary.t))): list(list(Secondary.t)) =>
    switch (wss) {
    | [] => []
    | [[w, ...ws], ...wss] when Secondary.is_space(w) => List.cons(ws, wss)
    | [ws, ...wss] => List.cons(ws, rm_up_to_one_space(wss))
    };

  let scooch_over_linebreak = wss' =>
    switch (wss') {
    | [ws, [w, ...ws'], ...wss] when Secondary.is_linebreak(w) => [
        ws @ [w],
        ws',
        ...wss,
      ]
    | _ => wss'
    };

  let add_grout = (shape: Nib.Shape.t, trim: t): IdGen.t(t) => {
    open IdGen.Syntax;
    let+ g = Grout.mk_fits_shape(shape);
    let (wss, gs) = trim;

    /* If we're adding a grout, remove a secondary. Note that
       changes made to the logic here should also take into
       account the other direction in 'regrout' below. */
    let trim = (g.shape == Concave ? rm_up_to_one_space(wss) : wss, gs);
    let (wss', gs') = cons_g(g, trim);
    /* Hack to supress the addition of leading secondary on a line */
    //let wss' = scooch_over_linebreak(wss');
    /* ANDREW: disabled above hack; with calmer indent it seems annoying */
    (wss', gs');
  };

  // assumes grout in trim fit r but may not fit l
  let regrout = ((l, r): Nibs.shapes, trim: t): IdGen.t(t) =>
    if (Nib.Shape.fits(l, r)) {
      let (wss, gs) = trim;

      /* Convert unneeded grout to spaces. Note that changes made
         to the logic here should also take into account the
         conversion of spaces to grout in 'add_grout' above. */
      let new_spaces =
        List.filter_map(
          ({id, shape}: Grout.t) =>
            switch (shape) {
            | Concave => Some(Secondary.mk_space(id))
            | Convex => None
            },
          gs,
        ); /* Note below that it is important that we add the new spaces
         before the existing wss, as doing otherwise may result
         in the new spaces ending up leading a line. This approach is
         somewhat hacky; we may just want to remove all the spaces
         whenever there is a linebreak; not making this chance now
         as I'm worried about it introducing subtle jank */

      /* David PR comment:
         All these changes assume the trim is ordered left-to-right,
         but this may not be true when Trim.regrout is called by
         regrout_affix(Left, ...) below, which reverses the affix before
         processing. (This didn't pose an issue before with trim because
         the secondary and grout are symmetric and the existing code
         didn't affect order.)
         Proper fix would require threading through directional parameter
         from regrout_affix into Trim.regrout and appending to correct side.
         Similar threading for add_grout. That said, I couldn't trigger any
         undesirable behavior with these changes and am fine with going ahead
         with this for now. */
      let wss = [new_spaces @ List.concat(wss)];
      IdGen.return(Aba.mk(wss, []));
    } else {
      let (_, gs) as merged = merge(trim);
      switch (gs) {
      | [] => add_grout(l, merged)
      | [_, ..._] => IdGen.return(merged)
      };
    };

  let to_seg = (trim: t) =>
    trim
    |> Aba.join(List.map(Piece.secondary), g => [Piece.Grout(g)])
    |> List.concat;
};

let rec regrout = ((l, r), seg) => {
  open IdGen.Syntax;
  let* (trim, r, tl) = regrout_affix(Direction.Right, seg, r);
  let+ trim = Trim.regrout((l, r), trim);
  Trim.to_seg(trim) @ tl;
}
and regrout_affix =
    (d: Direction.t, affix: t, r: Nib.Shape.t)
    : IdGen.t((Trim.t, Nib.Shape.t, t)) => {
  open IdGen.Syntax;
  let+ (trim, s, affix) =
    fold_right(
      (p: Piece.t, id_gen) => {
        let* (trim, r, tl) = id_gen;
        switch (p) {
        | Livelit(_) => IdGen.return((trim, r, tl)) // TODO Livelit
        | Secondary(w) => IdGen.return((Trim.cons_w(w, trim), r, tl))
        | Grout(g) => IdGen.return((Trim.(merge(cons_g(g, trim))), r, tl))
        | Tile(t) =>
          let* children =
            List.fold_right(
              (hd, tl) => {
                let* tl = tl;
                let+ hd = regrout(Nib.Shape.(concave(), concave()), hd);
                [hd, ...tl];
              },
              t.children,
              IdGen.return([]),
            );
          let p = Piece.Tile({...t, children});
          let (l', r') =
            Tile.shapes(t) |> (d == Left ? TupleUtil.swap : Fun.id);
          let+ trim = Trim.regrout((r', r), trim);
          (Trim.empty, l', [p, ...Trim.to_seg(trim)] @ tl);
        };
      },
      (d == Left ? List.rev : Fun.id)(affix),
      IdGen.return((Aba.mk([[]], []), r, empty)),
    );
  d == Left ? (Trim.rev(trim), s, rev(affix)) : (trim, s, affix);
};

// for internal use when dealing with segments in reverse order (eg Affix.re)
// let flip_nibs =
//   List.map(
//     fun
//     | (Piece.Secondary(_) | Grout(_)) as p => p
//     | Tile(t) => Tile({...t, mold: Mold.flip_nibs(t.mold)}),
//   );

let split_by_matching = (id: Id.t): (t => Aba.t(t, Tile.t)) =>
  Aba.split(
    fun
    | Piece.Tile(t) when t.id == id => Either.R(t)
    | p => L(p),
  );

// module Match = Tile.Match.Make(Orientation.R);
let rec reassemble = (seg: t): t =>
  switch (incomplete_tiles(seg)) {
  | [] => seg
  | [t, ..._] =>
    switch (Aba.trim(split_by_matching(t.id, seg))) {
    | None => seg
    | Some((seg_l, match, seg_r)) =>
      let t = Tile.reassemble(match);
      let children = List.map(reassemble, t.children);
      let p = Tile.to_piece({...t, children});
      seg_l @ [p, ...reassemble(seg_r)];
    }
  };

let trim_f: (list(Base.piece) => list(Base.piece), Direction.t, t) => t =
  (trim_l, d, ps) => {
    switch (d) {
    | Left => ps |> trim_l
    | Right => ps |> List.rev |> trim_l |> List.rev
    };
  };

let trim_secondary: (Direction.t, t) => t =
  (d, ps) => {
    /* Trims leading/trailing secondary */
    let rec trim_l = xs =>
      switch (xs) {
      | [] => []
      | [Piece.Secondary(_), ...xs] => trim_l(xs)
      | [_, ..._] => xs
      };
    trim_f(trim_l, d, ps);
  };

let trim_secondary_and_grout: (Direction.t, t) => t =
  (d, ps) => {
    /* Trims leading/trailing secondary, continuing
       to trim around grout until first Tile is reached */
    let rec trim_l: list(Base.piece) => list(Base.piece) =
      xs =>
        switch (xs) {
        | [] => []
        | [Secondary(_) | Grout(_), ...xs] => trim_l(xs)
        | [_, ..._] => xs
        };
    trim_f(trim_l, d, ps);
  };

let trim_grout_around_secondary: (Direction.t, t) => t =
  (d, ps) => {
    /* Trims leading/trailing grout, skipping over secondary,
       but not skipping over other pieces. */
    let rec trim_l: list(Base.piece) => list(Base.piece) =
      xs =>
        switch (xs) {
        | [] => []
        | [Secondary(w), ...xs] => [Secondary(w), ...trim_l(xs)]
        | [Grout(_), ...xs] => trim_l(xs)
        | [_, ..._] => xs
        };
    trim_f(trim_l, d, ps);
  };

let edge_shape_of = (d: Direction.t, ps: t): option(Nib.Shape.t) => {
  let trimmed = trim_secondary(d, ps);
  switch (d, ListUtil.hd_opt(trimmed), ListUtil.last_opt(trimmed)) {
  | (Right, _, Some(p)) => p |> Piece.shapes |> Option.map(snd)
  | (Left, Some(p), _) => p |> Piece.shapes |> Option.map(fst)
  | _ => None
  };
};

let edge_direction_of = (d: Direction.t, ps: t): option(Direction.t) =>
  Option.map(Nib.Shape.absolute(d), edge_shape_of(d, ps));

let rec serialize = (seg: t) =>
  seg
  |> List.concat_map(
       fun
       | (
           Piece.Secondary(_) | Livelit(_) | Grout(_) | Tile({shards: [_], _})
         ) as p => [
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

let sameline_secondary =
  List.for_all(
    fun
    | Piece.Secondary(w) => !Secondary.is_linebreak(w)
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
