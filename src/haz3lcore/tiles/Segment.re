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
      | Secondary(w) =>
        let (wss, gs) = wgw;
        let (ws, wss) = ListUtil.split_first(wss);
        (([[w, ...ws], ...wss], gs), s, tl);
      | Grout(g) => (Aba.cons([], g, wgw), s, tl)
      | Projector(p) =>
        let (l, _) =
          ProjectorBase.shapes(p) |> (d == Left ? TupleUtil.swap : Fun.id);
        (empty_wgw, l, tl);
      | Tile(t) =>
        let (l, _) = Tile.shapes(t) |> (d == Left ? TupleUtil.swap : Fun.id);
        (empty_wgw, l, tl);
      };
    };
  go((d == Left ? List.rev : Fun.id)(affix), r);
};

let rec remold = (~shape=Nib.Shape.concave(), seg: t, s: Sort.t) =>
  switch (s) {
  | Any => seg
  | Typ => remold_typ(shape, seg)
  | Pat => remold_pat(shape, seg)
  | Exp => remold_exp(shape, seg)
  | Rul => remold_rul(shape, seg)
  | TPat => remold_tpat(shape, seg)
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
              && List.nth(remolded.mold.in_, l) != List.nth(t.mold.in_, l)) {
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
and remold_typ = (shape, seg: t): t =>
  switch (seg) {
  | [] => []
  | [hd, ...tl] =>
    switch (hd) {
    | Secondary(_)
    | Grout(_)
    | Projector(_) => [hd, ...remold_typ(shape, tl)]
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
    | Secondary(_)
    | Grout(_)
    | Projector(_) =>
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
    | Secondary(_)
    | Grout(_)
    | Projector(_) =>
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
    | Secondary(_)
    | Grout(_)
    | Projector(_) => [hd, ...remold_pat(shape, tl)]
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
and remold_tpat_uni = (shape, seg: t): (t, Nib.Shape.t, t) =>
  switch (seg) {
  | [] => ([], shape, [])
  | [hd, ...tl] =>
    switch (hd) {
    | Secondary(_)
    | Grout(_)
    | Projector(_) =>
      let (remolded, shape, rest) = remold_tpat_uni(shape, tl);
      ([hd, ...remolded], shape, rest);
    | Tile(t) =>
      switch (remold_tile(TPat, shape, t)) {
      | None => ([], shape, seg)
      | Some(t) when !Tile.has_end(Right, t) =>
        let (_, r) = Tile.nibs(t);
        let remolded = remold(~shape=r.shape, tl, r.sort);
        let (_, shape, _) = shape_affix(Left, remolded, r.shape);
        ([Tile(t), ...remolded], shape, []);
      | Some(t) =>
        switch (Tile.nibs(t)) {
        | _ =>
          let (remolded, shape, rest) =
            remold_tpat_uni(snd(Tile.shapes(t)), tl);
          ([Tile(t), ...remolded], shape, rest);
        }
      }
    }
  }
and remold_tpat = (shape, seg: t): t =>
  switch (seg) {
  | [] => []
  | [hd, ...tl] =>
    switch (hd) {
    | Secondary(_)
    | Grout(_)
    | Projector(_) => [hd, ...remold_tpat(shape, tl)]
    | Tile(t) =>
      switch (remold_tile(TPat, shape, t)) {
      | None => [Tile(t), ...remold_tpat(snd(Tile.shapes(t)), tl)]
      | Some(t) =>
        switch (Tile.nibs(t)) {
        | (_, {shape, sort: Typ}) =>
          let (remolded, shape, rest) = remold_typ_uni(shape, tl);
          [Piece.Tile(t), ...remolded] @ remold_tpat(shape, rest);
        | _ => [Tile(t), ...remold_tpat(snd(Tile.shapes(t)), tl)]
        }
      }
    }
  }
and remold_exp_uni = (shape, seg: t): (t, Nib.Shape.t, t) =>
  switch (seg) {
  | [] => ([], shape, [])
  | [hd, ...tl] =>
    switch (hd) {
    | Secondary(_)
    | Grout(_)
    | Projector(_) =>
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
        | (_, {shape, sort: TPat}) =>
          let (remolded_tpat, shape, rest) = remold_tpat_uni(shape, tl);
          let (remolded_exp, shape, rest) = remold_exp_uni(shape, rest);
          ([Piece.Tile(t), ...remolded_tpat] @ remolded_exp, shape, rest);
        | (_, {shape, sort: Pat}) =>
          let (remolded_pat, shape, rest) = remold_pat_uni(shape, tl);
          let (remolded_exp, shape, rest) = remold_exp_uni(shape, rest);
          ([Piece.Tile(t), ...remolded_pat] @ remolded_exp, shape, rest);
        | (_, {shape, sort: Typ}) =>
          let (remolded_typ, shape, rest) = remold_typ_uni(shape, tl);
          let (remolded_exp, shape, rest) = remold_exp_uni(shape, rest);
          ([Piece.Tile(t), ...remolded_typ] @ remolded_exp, shape, rest);
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
    | Secondary(_)
    | Grout(_)
    | Projector(_) => [hd, ...remold_rul(shape, tl)]
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
    | Secondary(_)
    | Grout(_)
    | Projector(_) => [hd, ...remold_exp(shape, tl)]
    | Tile(t) =>
      switch (remold_tile(Exp, shape, t)) {
      | None => [Tile(t), ...remold_exp(snd(Tile.shapes(t)), tl)]
      | Some(t) =>
        switch (Tile.nibs(t)) {
        | (_, {shape, sort: Pat}) =>
          let (remolded, shape, rest) = remold_pat_uni(shape, tl);
          [Piece.Tile(t), ...remolded] @ remold_exp(shape, rest);
        | (_, {shape, sort: TPat}) =>
          let (remolded, shape, rest) = remold_tpat_uni(shape, tl);
          [Piece.Tile(t), ...remolded] @ remold_exp(shape, rest);
        | (_, {shape, sort: Typ}) =>
          let (remolded, shape, rest) = remold_typ_uni(shape, tl);
          [Piece.Tile(t), ...remolded] @ remold_exp(shape, rest);
        | (_, {shape, sort: Rul}) => [Tile(t), ...remold_rul(shape, tl)]
        | _ => [Tile(t), ...remold_exp(snd(Tile.shapes(t)), tl)]
        }
      }
    }
  };

let skel =
  Core.Memo.general(~cache_size_bound=10000, seg =>
    seg
    |> List.mapi((i, p) => (i, p))
    |> List.filter(((_, p)) => !Piece.is_secondary(p))
    |> Skel.mk
  );

let sorted_children = List.concat_map(Piece.sorted_children);
let children = seg => List.map(snd, sorted_children(seg));

module Trim = {
  type seg = t;
  type t = Aba.t(list(Secondary.t), Grout.t);

  let empty = Aba.mk([[]], []);

  let rev = Aba.rev(List.rev, Fun.id);

  let cons_w = (w: Secondary.t, (wss, gs)) => {
    // safe bc Aba always has at least one A element
    let (ws, wss) = ListUtil.split_first(wss);
    Aba.mk([[w, ...ws], ...wss], gs);
  };
  let cons_g = (g: Grout.t, (wss, gs)) =>
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

  let rec rm_up_to_one_space =
          (wss: list(list(Secondary.t))): list(list(Secondary.t)) =>
    switch (wss) {
    | [] => []
    | [[w, ...ws], ...wss] when Secondary.is_space(w) => List.cons(ws, wss)
    | [ws, ...wss] => List.cons(ws, rm_up_to_one_space(wss))
    };

  let add_grout = (~d: Direction.t, shape: Nib.Shape.t, (wss, gs): t): t => {
    let g = Grout.mk_fits_shape(shape);
    /* When adding a grout to the left, consume a space. Note
       changes made to the logic here should also take into
       account the other direction in 'regrout' below */
    let wss' =
      switch (d) {
      /* Right Convex e.g. Backspace `1| + 2` => `|<> + 2` (Don't consume) */
      /* Right Concave e.g. Backspace `1 +| 1` => `1 |>< 1` (Don't consume) */
      | Right => wss
      /* Left Convex e.g. Insert Space `[|]` => `[ |]` => `[<>|]` (Consume) */
      /* Left Concave e.g. Insert "i" `let a = 1 i|` => `let a = 1><i|` (Consume) */
      | Left => rm_up_to_one_space(wss)
      };
    cons_g(g, (wss', gs));
  };

  // assumes grout in trim fit r but may not fit l
  let regrout = (d: Direction.t, (l, r): Nibs.shapes, trim: t): t =>
    if (Nib.Shape.fits(l, r)) {
      let (wss, gs) = trim;
      /* When removing a grout to the Left, add a space. Note
         changes made to the logic here should also take into
         account the other direction in 'add_grout' above */
      let new_spaces =
        List.filter_map(
          (g: Grout.t) => {
            switch (g.shape, d) {
            /* Left Concave e.g. `let a = 1><in|` => `let a = 1 in |` (Add) */
            /* NOTE(andrew): Not sure why d here seems reversed. Also not sure why
             * restriction to concave is necessary but seems to prevent addition
             * of needless whitespace in some situation such as when inserting
             * on `(|)`, which seems to add whitespace after the right parens
             * without this shape restirction */
            | (Concave, Right) => Some(Secondary.mk_space(g.id))
            | _ => None
            }
          },
          gs,
        );
      /* Note below that it is important that we add the new spaces
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
      Aba.mk(wss, []);
    } else {
      let (_, gs) as merged = merge(trim);
      switch (gs) {
      | [] => add_grout(~d, l, merged)
      | [_, ..._] => merged
      };
    };

  let to_seg = (trim: t) =>
    trim
    |> Aba.join(List.map(Piece.secondary), g => [Piece.Grout(g)])
    |> List.concat;
};

let rec regrout = ((l, r), seg) => {
  let (trim, r, tl) = regrout_affix(Direction.Right, seg, r);
  let trim = Trim.regrout(Direction.Right, (l, r), trim);
  Trim.to_seg(trim) @ tl;
}
and regrout_affix =
    (d: Direction.t, affix: t, r: Nib.Shape.t): (Trim.t, Nib.Shape.t, t) => {
  let (trim, s, affix) =
    fold_right(
      (p: Piece.t, (trim, r, tl)) => {
        switch (p) {
        | Secondary(w) => (Trim.cons_w(w, trim), r, tl)
        | Grout(g) => (Trim.(merge(cons_g(g, trim))), r, tl)
        | Projector(pr) =>
          let p = Piece.Projector(pr);
          let (l', r') =
            ProjectorBase.shapes(pr) |> (d == Left ? TupleUtil.swap : Fun.id);
          let trim = Trim.regrout(d, (r', r), trim);
          (Trim.empty, l', [p, ...Trim.to_seg(trim)] @ tl);
        | Tile(t) =>
          let children =
            List.fold_right(
              (hd, tl) => {
                let tl = tl;
                let hd = regrout(Nib.Shape.(concave(), concave()), hd);
                [hd, ...tl];
              },
              t.children,
              [],
            );
          let p = Piece.Tile({...t, children});
          let (l', r') =
            Tile.shapes(t) |> (d == Left ? TupleUtil.swap : Fun.id);
          let trim = Trim.regrout(d, (r', r), trim);
          (Trim.empty, l', [p, ...Trim.to_seg(trim)] @ tl);
        }
      },
      (d == Left ? List.rev : Fun.id)(affix),
      (Aba.mk([[]], []), r, empty),
    );
  d == Left ? (Trim.rev(trim), s, rev(affix)) : (trim, s, affix);
};

let split_by_matching = (id: Id.t): (t => Aba.t(t, Tile.t)) =>
  Aba.split(
    fun
    | Piece.Tile(t) when t.id == id => Either.R(t)
    | p => L(p),
  );

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

let rec holes = (segment: t): list(Grout.t) =>
  List.concat_map(
    fun
    | Piece.Secondary(_)
    | Projector(_) => []
    | Tile(t) => List.concat_map(holes, t.children)
    | Grout(g) => [g],
    segment,
  );

let get_childrens: t => list(t) =
  List.concat_map(
    fun
    | Piece.Tile(t) => t.children
    | _ => [],
  );

let rec get_incomplete_ids = (seg: t): list(Id.t) =>
  List.concat_map(
    fun
    | Piece.Tile(t) => {
        let ids = List.concat_map(get_incomplete_ids, t.children);
        Tile.is_complete(t) ? ids : [t.id, ...ids];
      }
    | _ => [],
    seg,
  );

let ids_of_incomplete_tiles_in_bidelimiteds = (seg: t): list(Id.t) =>
  get_childrens(seg) |> List.concat |> get_incomplete_ids;

let rec ids = (s: t): list(Id.t) => List.concat_map(ids_of_piece, s)
and ids_of_piece = (p: Piece.t): list(Id.t) =>
  switch (p) {
  | Tile(t) => [Piece.id(p), ...ids(List.concat(t.children))]
  | Grout(_)
  | Secondary(_)
  | Projector(_) => [Piece.id(p)]
  };
