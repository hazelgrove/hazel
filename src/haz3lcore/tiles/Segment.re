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
  Effect.s_remold(t.id, t.mold, remolded.mold);
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
            remold(child, List.nth(remolded.mold.in_, l).sort);
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
    | Whitespace(_)
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
    | Whitespace(_)
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
    | Whitespace(_)
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
    | Whitespace(_)
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
    | Whitespace(_)
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
    | Whitespace(_)
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
    | Whitespace(_)
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
  |> List.filter(((_, p)) => !Piece.is_whitespace(p))
  |> Skel.mk;

let nibbed_children = seg =>
  seg |> List.map(Piece.nibbed_children) |> List.concat;
let children = seg => List.map(snd, nibbed_children(seg));

module Trim = {
  type seg = t;
  [@deriving show({with_path: false})]
  type t = Aba.t(list(Whitespace.t), Grout.t);

  let empty = Aba.mk([[]], []);

  let rev = Aba.rev(List.rev, Fun.id);

  let length = trim =>
    trim |> Aba.join(List.length, _ => 1) |> List.fold_left((+), 0);

  let append: (t, t) => t = Aba.append((@));

  let cons_w = (w: Whitespace.t, (wss, gs)) => {
    // safe bc Aba always has at least one A element
    let (ws, wss) = ListUtil.split_first(wss);
    Aba.mk([[w, ...ws], ...wss], gs);
  };
  let cons_g = (g: Grout.t, (wss, gs)) =>
    Aba.mk([[], ...wss], [g, ...gs]);

  let ws = ((wss, _): t): seg => List.(map(Piece.whitespace, concat(wss)));

  // TODO clean up l_pad bool in return type
  let repad = (l_pad, trim: t, r_pad): IdGen.t((bool, t)) =>
    IdGen.Syntax.(
      switch (trim) {
      | ([ws], []) =>
        (l_pad || r_pad) && ws == []
          ? {
            let+ space = Whitespace.mk_space;
            (true, ([[space]], []));
          }
          : return((false, trim))
      | _ =>
        let* (l_padded, trim) =
          l_pad && Aba.first_a(trim) == []
            ? {
              let+ space = Whitespace.mk_space;
              (true, append(([[space]], []), trim));
            }
            : return((false, trim));
        let+ trim =
          r_pad && Aba.last_a(trim) == []
            ? {
              let+ space = Whitespace.mk_space;
              append(trim, ([[space]], []));
            }
            : return(trim);
        (l_padded, trim);
      }
    );

  let regrout =
      (
        ~lint=true,
        ~caret: option(int)=?,
        (l, r): Nibs.t,
        trim: t,
        s: Sort.t,
      )
      : IdGen.t((int, t)) => {
    // index each element of the original trim to determine change
    // in caret index after regrouting
    let (_, itrim) =
      trim
      |> Aba.fold_left_map(
           ws => (List.length(ws), List.mapi((i, w) => (i, w), ws)),
           (i, g, ws) => {
             let n = List.length(ws);
             (i + 1 + n, (i, g), List.mapi((j, w) => (i + 1 + j, w), ws));
           },
         );
    open IdGen.Syntax;
    let* new_gs = Grout.mk((l, r), s);
    let (remaining_gs, new_itrim) =
      itrim
      |> Aba.map_a(
           List.filter(((_, w)) => !lint || Whitespace.is_linebreak(w)),
         )
      |> Aba.fold_left_map(
           ws => (new_gs, ws),
           (new_gs, (i, _), ws) =>
             switch (new_gs) {
             | [] => ([], None, ws)
             | [hd, ...tl] => (tl, Some((i, hd)), ws)
             },
         );
    let new_itrim: Aba.t(list((int, Whitespace.t)), (int, Grout.t)) =
      new_itrim
      |> Aba.fold_right(
           (ws, g, trim) =>
             switch (g, trim) {
             | (None, ([hd, ...tl], gs)) => Aba.mk([ws @ hd, ...tl], gs)
             | (Some(g), _) => Aba.cons(ws, g, trim)
             | _ => raise(Aba.Invalid)
             },
           ws => Aba.mk([ws], []),
         );

    let lt_caret = i =>
      switch (caret) {
      | None => false
      | Some(j) => i < j
      };

    let new_itrim_with_extra_gs:
      Aba.t(list((int, Whitespace.t)), (int, Grout.t)) = {
      let cons_remaining_gs = (trim: Aba.t(_)) =>
        List.fold_right(
          // HACK(d) -1 index safe because this will only happen after caret
          (g, trim) => Aba.cons([], ((-1), g), trim),
          remaining_gs,
          trim,
        );

      let go_iws = (iws, (consed, trim: Aba.t(_))) =>
        List.fold_right(
          ((i, _) as iw, (consed, trim)) => {
            let (consed, trim) =
              lt_caret(i) && !consed
                ? (true, cons_remaining_gs(trim)) : (consed, trim);
            (consed, Aba.append((@), ([[iw]], []), trim));
          },
          iws,
          (consed, trim),
        );

      let (consed, new_itrim) =
        new_itrim
        |> Aba.fold_right(
             (iws, ig, (consed, trim: Aba.t(_))) => {
               let trim = consed ? trim : cons_remaining_gs(trim);
               let trim = Aba.cons([], ig, trim);
               go_iws(iws, (true, trim));
             },
             iws => go_iws(iws, (false, ([[]], []))),
           );
      consed ? new_itrim : cons_remaining_gs(new_itrim);
    };

    let caret =
      new_itrim_with_extra_gs
      |> Aba.map_a(List.map(((i, _)) => lt_caret(i) ? 1 : 0))
      // HACK(d): i >= 0 to account for negative index hack above
      |> Aba.map_b(((i, _)) => i >= 0 && lt_caret(i) ? 1 : 0)
      |> Aba.join(List.fold_left((+), 0), Fun.id)
      |> List.fold_left((+), 0);

    let new_trim =
      new_itrim_with_extra_gs |> Aba.map_a(List.map(snd)) |> Aba.map_b(snd);

    // let new_trim_with_extra_gs =
    //   remaining_gs
    //   |> List.fold_left((trim, g) => Aba.snoc(trim, g, []), new_trim);

    let+ (l_padded, padded_trim) =
      lint
        ? repad(Nib.is_padded(l), new_trim, Nib.is_padded(r))
        : return((false, new_trim));

    (caret + (l_padded ? 1 : 0), padded_trim);
  };

  let is_linted = (nibs, trim, s) => {
    let ((_, trim'), _) = regrout(nibs, trim, s, 0);
    switch (Aba.zip_opt(trim, trim')) {
    | None => false
    | Some((wswss, _)) =>
      wswss
      |> List.for_all(((ws, ws')) => List.length(ws) == List.length(ws'))
    };
  };

  let to_seg = (trim: t) =>
    trim
    |> Aba.join(List.map(Piece.whitespace), g => [Piece.Grout(g)])
    |> List.concat;
};

let rec regrout = ((l, r): Nibs.t, seg, s: Sort.t) => {
  open IdGen.Syntax;
  let* (trim, r, tl) = regrout_affix(Direction.Right, seg, r, s);
  let+ (_, trim) = Trim.regrout((l, r), trim, s);
  Trim.to_seg(trim) @ tl;
}
and regrout_affix =
    (d: Direction.t, affix: t, r: Nib.t, s: Sort.t)
    : IdGen.t((Trim.t, Nib.t, t)) => {
  open IdGen.Syntax;
  let+ (trim, s, affix) =
    fold_right(
      (p: Piece.t, id_gen) => {
        let* (trim, r, tl) = id_gen;
        switch (p) {
        | Whitespace(w) => IdGen.return((Trim.cons_w(w, trim), r, tl))
        | Grout(g) => IdGen.return((Trim.cons_g(g, trim), r, tl))
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
          let+ (_, trim) = Trim.regrout((r', r), trim, s);
          (Trim.empty, l', [p, ...Trim.to_seg(trim)] @ tl);
        };
      },
      (d == Left ? List.rev : Fun.id)(affix),
      IdGen.return((Aba.mk([[]], []), r, empty)),
    );
  d == Left ? (Trim.rev(trim), s, rev(affix)) : (trim, s, affix);
};

let split_by_matching = (id: Id.t): (t => Aba.t(t, Tile.t)) =>
  Aba.split(
    fun
    | Piece.Tile(t) when t.id == id => Either.R(t)
    | p => L(p),
  );

let matches = (l: Tile.t, r: Tile.t): bool =>
  l.label == r.label && Tile.r_shard(l) + 1 == Tile.l_shard(r);

type split = Aba.t(t, Tile.t);

let find_match = (t: Tile.t, split: split): option((Tile.t, split)) => {
  let rec go = (popped, split): option((Tile.t, split)) => {
    open OptUtil.Syntax;
    let* (seg, t', split) = Aba.pop_ab(split);
    if (matches(t, t')) {
      let t = {
        ...t,
        shards: t.shards @ t'.shards,
        children: t.children @ [popped, ...t'.children],
      };
      Some((t, split));
    } else {
      go(concat([seg, [Piece.Tile(t'), ...popped]]), split);
    };
  };
  go(empty, split);
};

let rec reassemble_split = (seg: t): Aba.t(t, Tile.t) =>
  switch (seg) {
  | [] => Aba.mk([seg], [])
  | [hd, ...tl] =>
    let split = reassemble_split(tl);
    let cons_complete = (p, split) => Aba.update_first_a(cons(p), split);
    let cons_incomplete = (t, split) => Aba.cons(empty, t, split);
    switch (hd) {
    | Whitespace(_)
    | Grout(_) => cons_complete(hd, split)
    | Tile(t) when Tile.is_complete(t) => cons_complete(hd, split)
    | Tile(t) =>
      switch (find_match(t, split)) {
      | None => cons_incomplete(t, split)
      | Some((t, split)) =>
        Tile.is_complete(t)
          ? cons_complete(Piece.Tile(t), split) : cons_incomplete(t, split)
      }
    };
  };

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
  |> List.map(
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
     )
  |> List.concat;

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
