open Util;

exception Empty_segment;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
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

let rec incomplete_tiles_deep = (seg: t) =>
  List.map(
    fun
    | Piece.Tile(t) =>
      (!Tile.is_complete(t) ? [t] : [])
      @ List.concat_map(incomplete_tiles_deep, t.children)
    | _ => [],
    seg,
  )
  |> List.concat;

let incomplete_tiles_to_missing_shards = seg =>
  seg |> List.map(Tile.missing_shards) |> List.concat;

let global_missing_shards = (seg: t) =>
  seg |> incomplete_tiles_deep |> incomplete_tiles_to_missing_shards;

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
      let shape =
        switch (Piece.shapes(p)) {
        | Some(shapes) =>
          shapes |> (d == Left ? TupleUtil.swap : Fun.id) |> fst
        | None => s
        };
      switch (p) {
      | Secondary(w) =>
        let (wss, gs) = wgw;
        let (ws, wss) = ListUtil.split_first(wss);
        (([[w, ...ws], ...wss], gs), shape, tl);
      | Grout(g) => (Aba.cons([], g, wgw), shape, tl)
      | Projector(_) => (empty_wgw, shape, tl)
      | Tile(_) => (empty_wgw, shape, tl)
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
  | Mod => remold_mod(shape, seg)
  }
and remold_tile = (s: Sort.t, shape, t: Tile.t): option(Tile.t) => {
  open OptUtil.Syntax;
  let+ remolded =
    switch (Form.Molds.try_get(s, t.label)) {
    | None => None
    | Some(molds) =>
      molds
      |> List.map(mold =>
           {
             ...t,
             mold,
           }
         )
      |> (
        fun
        | [_] as ts => ts
        | ts =>
          ts |> List.filter(t => Nib.Shape.fits(shape, fst(Tile.shapes(t))))
      )
      |> ListUtil.hd_opt
    };
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
  {
    ...remolded,
    children,
  };
}
and remold_typ = (shape, seg: t): t =>
  switch (seg) {
  | [] => []
  | [hd, ...tl] =>
    switch (hd) {
    | Secondary(_)
    | Grout(_) => [hd, ...remold_typ(shape, tl)]
    | Projector(p) => [hd, ...remold_typ(snd(ProjectorCore.shapes(p)), tl)]
    | Tile(t) =>
      switch (remold_tile(Typ, shape, t)) {
      | None => [Tile(t), ...remold_typ(snd(Tile.shapes(t)), tl)]
      | Some(t) when !Tile.has_end(Right, t) =>
        let (_, r) = Tile.nibs(t);
        let remolded = remold(~shape=r.shape, tl, r.sort);
        [Tile(t), ...remolded];
      | Some(t) => [Tile(t), ...remold_typ(snd(Tile.shapes(t)), tl)]
      }
    }
  }
and remold_typ_uni = (shape, seg: t, parent_sorts): (t, Nib.Shape.t, t) =>
  switch (seg) {
  | [] => ([], shape, [])
  | [hd, ...tl] =>
    switch (hd) {
    | Secondary(_)
    | Grout(_) =>
      let (remolded, shape, rest) = remold_typ_uni(shape, tl, parent_sorts);
      ([hd, ...remolded], shape, rest);
    | Projector(p) =>
      let (remolded, shape, rest) =
        remold_typ_uni(snd(ProjectorCore.shapes(p)), tl, parent_sorts);
      ([hd, ...remolded], shape, rest);
    | Tile(t) =>
      /* If we encounter ; and Mod is a parent sort, return to let Mod handle it as ModSeq.
         This handles cases like `type t = Int;` where ; follows a Typ child of ModType. */
      switch (remold_tile(Typ, shape, t)) {
      | None
          when t.label == [";"] && List.exists((==)(Sort.Mod), parent_sorts) => (
          [],
          shape,
          seg,
        )
      | None => ([], shape, seg)
      | Some(t) when !Tile.has_end(Right, t) =>
        let (_, r) = Tile.nibs(t);
        let remolded = remold(~shape=r.shape, tl, r.sort);
        let (_, shape, _) = shape_affix(Left, remolded, r.shape);
        ([Tile(t), ...remolded], shape, []);
      | Some(t)
          when
            t.label == Form.get(CommaTyp).label
            || t.label == Form.get(TypPlus).label
            && List.exists((==)(Sort.Exp), parent_sorts) => (
          [],
          shape,
          seg,
        )
      | Some(t) =>
        let (remolded, shape, rest) =
          remold_typ_uni(snd(Tile.shapes(t)), tl, parent_sorts);
        ([Tile(t), ...remolded], shape, rest);
      }
    }
  }
and remold_pat_uni = (shape, seg: t, parent_sorts): (t, Nib.Shape.t, t) =>
  switch (seg) {
  | [] => ([], shape, [])
  | [hd, ...tl] =>
    switch (hd) {
    | Secondary(_)
    | Grout(_) =>
      let (remolded, shape, rest) = remold_pat_uni(shape, tl, parent_sorts);
      ([hd, ...remolded], shape, rest);
    | Projector(p) =>
      let (remolded, shape, rest) =
        remold_pat_uni(snd(ProjectorCore.shapes(p)), tl, parent_sorts);
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
          let (remolded_typ, shape, rest) =
            remold_typ_uni(shape, tl, [Sort.Pat, ...parent_sorts]);
          let (remolded_pat, shape, rest) =
            remold_pat_uni(shape, rest, parent_sorts);
          ([Piece.Tile(t), ...remolded_typ] @ remolded_pat, shape, rest);
        | _ =>
          let (remolded, shape, rest) =
            remold_pat_uni(snd(Tile.shapes(t)), tl, parent_sorts);
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
    | Grout(_) => [hd, ...remold_pat(shape, tl)]
    | Projector(p) => [hd, ...remold_pat(snd(ProjectorCore.shapes(p)), tl)]
    | Tile(t) =>
      switch (remold_tile(Pat, shape, t)) {
      | None => [Tile(t), ...remold_pat(snd(Tile.shapes(t)), tl)]
      | Some(t) when !Tile.has_end(Right, t) =>
        let (_, r) = Tile.nibs(t);
        let remolded = remold(~shape=r.shape, tl, r.sort);
        [Tile(t), ...remolded];
      | Some(t) =>
        switch (Tile.nibs(t)) {
        | (_, {shape, sort: Typ}) =>
          let (remolded, shape, rest) =
            remold_typ_uni(shape, tl, [Sort.Pat]);
          [Piece.Tile(t), ...remolded] @ remold_pat(shape, rest);
        | _ => [Tile(t), ...remold_pat(snd(Tile.shapes(t)), tl)]
        }
      }
    }
  }
and remold_tpat_uni = (shape, seg: t, parent_sorts): (t, Nib.Shape.t, t) =>
  switch (seg) {
  | [] => ([], shape, [])
  | [hd, ...tl] =>
    switch (hd) {
    | Secondary(_)
    | Grout(_) =>
      let (remolded, shape, rest) = remold_tpat_uni(shape, tl, parent_sorts);
      ([hd, ...remolded], shape, rest);
    | Projector(p) =>
      let (remolded, shape, rest) =
        remold_tpat_uni(snd(ProjectorCore.shapes(p)), tl, parent_sorts);
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
            remold_tpat_uni(snd(Tile.shapes(t)), tl, parent_sorts);
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
    | Grout(_) => [hd, ...remold_tpat(shape, tl)]
    | Projector(p) => [
        hd,
        ...remold_tpat(snd(ProjectorCore.shapes(p)), tl),
      ]
    | Tile(t) =>
      switch (remold_tile(TPat, shape, t)) {
      | None => [Tile(t), ...remold_tpat(snd(Tile.shapes(t)), tl)]
      | Some(t) when !Tile.has_end(Right, t) =>
        let (_, r) = Tile.nibs(t);
        let remolded = remold(~shape=r.shape, tl, r.sort);
        [Tile(t), ...remolded];
      | Some(t) =>
        switch (Tile.nibs(t)) {
        | (_, {shape, sort: Typ}) =>
          let (remolded, shape, rest) =
            remold_typ_uni(shape, tl, [Sort.TPat]);
          [Piece.Tile(t), ...remolded] @ remold_tpat(shape, rest);
        | _ => [Tile(t), ...remold_tpat(snd(Tile.shapes(t)), tl)]
        }
      }
    }
  }
and remold_exp_uni = (shape, seg: t, parent_sorts): (t, Nib.Shape.t, t) =>
  switch (seg) {
  | [] => ([], shape, [])
  | [hd, ...tl] =>
    switch (hd) {
    | Secondary(_)
    | Grout(_) =>
      let (remolded, shape, rest) = remold_exp_uni(shape, tl, parent_sorts);
      ([hd, ...remolded], shape, rest);
    | Projector(p) =>
      let (remolded, shape, rest) =
        remold_exp_uni(snd(ProjectorCore.shapes(p)), tl, parent_sorts);
      ([hd, ...remolded], shape, rest);
    | Tile(t) =>
      switch (remold_tile(Exp, shape, t)) {
      | None => ([], shape, seg)
      | Some(t) when !Tile.has_end(Right, t) =>
        let (_, r) = Tile.nibs(t);
        let remolded = remold(~shape=r.shape, tl, r.sort);
        let (_, shape, _) = shape_affix(Left, remolded, r.shape);
        ([Tile(t), ...remolded], shape, []);
      /* If we encounter ; and Mod is a parent sort, return to let Mod handle it as ModSeq.
         NOTE: This creates a grammatical ambiguity - we're prioritizing ModSeq over CellJoin
         when inside a module context. This may not always be desired if someone wants an
         expression-level sequence inside a module. Future consideration: may want to remove
         Exp-level semicolon entirely or find a more principled disambiguation approach. */
      | Some(t)
          when
            t.label == Form.get(CellJoin).label
            && List.exists((==)(Sort.Mod), parent_sorts) => (
          [],
          shape,
          seg,
        )
      | Some(t) =>
        switch (Tile.nibs(t)) {
        | (_, {shape, sort: TPat}) =>
          let (remolded_tpat, shape, rest) =
            remold_tpat_uni(shape, tl, [Sort.Exp, ...parent_sorts]);
          let (remolded_exp, shape, rest) =
            remold_exp_uni(shape, rest, parent_sorts);
          ([Piece.Tile(t), ...remolded_tpat] @ remolded_exp, shape, rest);
        | (_, {shape, sort: Pat}) =>
          let (remolded_pat, shape, rest) =
            remold_pat_uni(shape, tl, [Sort.Exp, ...parent_sorts]);
          let (remolded_exp, shape, rest) =
            remold_exp_uni(shape, rest, parent_sorts);
          ([Piece.Tile(t), ...remolded_pat] @ remolded_exp, shape, rest);
        | (_, {shape, sort: Typ}) =>
          let (remolded_typ, shape, rest) =
            remold_typ_uni(shape, tl, [Sort.Exp, ...parent_sorts]);
          let (remolded_exp, shape, rest) =
            remold_exp_uni(shape, rest, parent_sorts);
          ([Piece.Tile(t), ...remolded_typ] @ remolded_exp, shape, rest);
        | (_, {shape, sort: Rul}) =>
          // TODO review short circuit
          ([Tile(t)], shape, tl)
        | _ =>
          let (remolded, shape, rest) =
            remold_exp_uni(snd(Tile.shapes(t)), tl, parent_sorts);
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
    | Grout(_) => [hd, ...remold_rul(shape, tl)]
    | Projector(p) => [hd, ...remold_rul(snd(ProjectorCore.shapes(p)), tl)]
    | Tile(t) =>
      switch (remold_tile(Rul, shape, t)) {
      | Some(t) when !Tile.has_end(Right, t) =>
        let (_, r) = Tile.nibs(t);
        let remolded = remold(~shape=r.shape, tl, r.sort);
        [Tile(t), ...remolded];
      | Some(t) =>
        switch (Tile.nibs(t)) {
        | (_, {shape, sort: Exp}) =>
          let (remolded, shape, rest) =
            remold_exp_uni(shape, tl, [Sort.Rul]);
          [Piece.Tile(t), ...remolded] @ remold_rul(shape, rest);
        | (_, {shape, sort: Pat}) =>
          let (remolded, shape, rest) =
            remold_pat_uni(shape, tl, [Sort.Rul]);
          // TODO(d) continuing onto rule might not be right right...
          [Piece.Tile(t), ...remolded] @ remold_rul(shape, rest);
        | _ => failwith("remold_rul unexpected")
        }
      | None =>
        // TODO: not sure whether we should add Rul to parent_sorts here
        let (remolded, shape, rest) =
          remold_exp_uni(shape, [hd, ...tl], []);
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
    | Grout(_) => [hd, ...remold_exp(shape, tl)]
    | Projector(p) => [hd, ...remold_exp(snd(ProjectorCore.shapes(p)), tl)]
    | Tile(t) =>
      switch (remold_tile(Exp, shape, t)) {
      | None => [Tile(t), ...remold_exp(snd(Tile.shapes(t)), tl)]
      | Some(t) when !Tile.has_end(Right, t) =>
        let (_, r) = Tile.nibs(t);
        let remolded = remold(~shape=r.shape, tl, r.sort);
        [Tile(t), ...remolded];
      | Some(t) =>
        switch (Tile.nibs(t)) {
        | (_, {shape, sort: Pat}) =>
          let (remolded, shape, rest) = remold_pat_uni(shape, tl, [Exp]);
          [Piece.Tile(t), ...remolded] @ remold_exp(shape, rest);
        | (_, {shape, sort: TPat}) =>
          let (remolded, shape, rest) = remold_tpat_uni(shape, tl, [Exp]);
          [Piece.Tile(t), ...remolded] @ remold_exp(shape, rest);
        | (_, {shape, sort: Typ}) =>
          let (remolded, shape, rest) = remold_typ_uni(shape, tl, [Exp]);
          [Piece.Tile(t), ...remolded] @ remold_exp(shape, rest);
        | (_, {shape, sort: Rul}) => [Tile(t), ...remold_rul(shape, tl)]
        | (_, {shape, sort: Mod}) =>
          let (remolded, shape, rest) =
            remold_mod_uni(shape, tl, [Sort.Exp]);
          [Piece.Tile(t), ...remolded] @ remold_exp(shape, rest);
        | _ => [Tile(t), ...remold_exp(snd(Tile.shapes(t)), tl)]
        }
      }
    }
  }
and remold_mod_uni = (shape, seg: t, parent_sorts): (t, Nib.Shape.t, t) =>
  switch (seg) {
  | [] => ([], shape, [])
  | [hd, ...tl] =>
    switch (hd) {
    | Secondary(_)
    | Grout(_) =>
      let (remolded, shape, rest) = remold_mod_uni(shape, tl, parent_sorts);
      ([hd, ...remolded], shape, rest);
    | Projector(p) =>
      let (remolded, shape, rest) =
        remold_mod_uni(snd(ProjectorCore.shapes(p)), tl, parent_sorts);
      ([hd, ...remolded], shape, rest);
    | Tile(t) =>
      switch (remold_tile(Mod, shape, t)) {
      | None => ([], shape, seg)
      | Some(t) when !Tile.has_end(Right, t) =>
        let (_, r) = Tile.nibs(t);
        let remolded = remold(~shape=r.shape, tl, r.sort);
        let (_, shape, _) = shape_affix(Left, remolded, r.shape);
        ([Tile(t), ...remolded], shape, []);
      | Some(t) =>
        switch (Tile.nibs(t)) {
        | (_, {shape, sort: Pat}) =>
          let (remolded_pat, shape, rest) =
            remold_pat_uni(shape, tl, [Sort.Mod, ...parent_sorts]);
          let (remolded_mod, shape, rest) =
            remold_mod_uni(shape, rest, parent_sorts);
          ([Piece.Tile(t), ...remolded_pat] @ remolded_mod, shape, rest);
        | (_, {shape, sort: TPat}) =>
          let (remolded_tpat, shape, rest) =
            remold_tpat_uni(shape, tl, [Sort.Mod, ...parent_sorts]);
          let (remolded_mod, shape, rest) =
            remold_mod_uni(shape, rest, parent_sorts);
          ([Piece.Tile(t), ...remolded_tpat] @ remolded_mod, shape, rest);
        | (_, {shape, sort: Typ}) =>
          let (remolded_typ, shape, rest) =
            remold_typ_uni(shape, tl, [Sort.Mod, ...parent_sorts]);
          let (remolded_mod, shape, rest) =
            remold_mod_uni(shape, rest, parent_sorts);
          ([Piece.Tile(t), ...remolded_typ] @ remolded_mod, shape, rest);
        | (_, {shape, sort: Exp}) =>
          let (remolded_exp, shape, rest) =
            remold_exp_uni(shape, tl, [Sort.Mod, ...parent_sorts]);
          let (remolded_mod, shape, rest) =
            remold_mod_uni(shape, rest, parent_sorts);
          ([Piece.Tile(t), ...remolded_exp] @ remolded_mod, shape, rest);
        | _ =>
          let (remolded, shape, rest) =
            remold_mod_uni(snd(Tile.shapes(t)), tl, parent_sorts);
          ([Tile(t), ...remolded], shape, rest);
        }
      }
    }
  }
and remold_mod = (shape, seg: t): t =>
  switch (seg) {
  | [] => []
  | [hd, ...tl] =>
    switch (hd) {
    | Secondary(_)
    | Grout(_) => [hd, ...remold_mod(shape, tl)]
    | Projector(p) => [hd, ...remold_mod(snd(ProjectorCore.shapes(p)), tl)]
    | Tile(t) =>
      switch (remold_tile(Mod, shape, t)) {
      | None =>
        /* No Mod form - try Exp since bare expressions are valid module items */
        switch (remold_tile(Exp, shape, t)) {
        | None => [Tile(t), ...remold_mod(snd(Tile.shapes(t)), tl)]
        | Some(t) =>
          let (remolded, shape, rest) =
            remold_exp_uni(snd(Tile.shapes(t)), tl, [Mod]);
          [Piece.Tile(t), ...remolded] @ remold_mod(shape, rest);
        }
      | Some(t) when !Tile.has_end(Right, t) =>
        let (_, r) = Tile.nibs(t);
        let remolded = remold(~shape=r.shape, tl, r.sort);
        [Tile(t), ...remolded];
      | Some(t) =>
        switch (Tile.nibs(t)) {
        | (_, {shape, sort: Pat}) =>
          let (remolded, shape, rest) = remold_pat_uni(shape, tl, [Mod]);
          [Piece.Tile(t), ...remolded] @ remold_mod(shape, rest);
        | (_, {shape, sort: TPat}) =>
          let (remolded, shape, rest) = remold_tpat_uni(shape, tl, [Mod]);
          [Piece.Tile(t), ...remolded] @ remold_mod(shape, rest);
        | (_, {shape, sort: Typ}) =>
          let (remolded, shape, rest) = remold_typ_uni(shape, tl, [Mod]);
          [Piece.Tile(t), ...remolded] @ remold_mod(shape, rest);
        | (_, {shape, sort: Exp}) =>
          let (remolded, shape, rest) = remold_exp_uni(shape, tl, [Mod]);
          [Piece.Tile(t), ...remolded] @ remold_mod(shape, rest);
        | _ => [Tile(t), ...remold_mod(snd(Tile.shapes(t)), tl)]
        }
      }
    }
  };

let skel =
  Core.Memo.general(~cache_size_bound=10000, seg => {
    seg
    |> List.mapi((i, p) => (i, p))
    |> List.filter(((_, p)) => !Piece.is_secondary(p))
    |> Skel.mk
  });

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

  let add_grout = (shape: Nib.Shape.t, (wss, gs): t): t =>
    cons_g(Grout.mk_fits_shape(shape), (wss, gs));

  // assumes grout in trim fit r but may not fit l
  let regrout = ((l, r): Nibs.shapes, trim: t): t =>
    if (Nib.Shape.fits(l, r)) {
      Aba.mk([List.concat(trim |> fst)], []);
    } else {
      let (_, gs) as merged = merge(trim);
      switch (gs) {
      | [] => add_grout(l, merged)
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
  let trim = Trim.regrout((l, r), trim);
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
          let (l', r') =
            ProjectorCore.shapes(pr) |> (d == Left ? TupleUtil.swap : Fun.id);
          let trim = Trim.regrout((r', r), trim);
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
          let p =
            Piece.Tile({
              ...t,
              children,
            });
          let (l', r') =
            Tile.shapes(t) |> (d == Left ? TupleUtil.swap : Fun.id);
          let trim = Trim.regrout((r', r), trim);
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

let inner_regrout = (children: list(t)): list(t) =>
  List.map(regrout((Nib.Shape.concave(), Nib.Shape.concave())), children);

let rec reassemble = (seg: t): t =>
  switch (incomplete_tiles(seg)) {
  | [] => seg
  | [t, ..._] =>
    switch (Aba.trim(split_by_matching(t.id, seg))) {
    | None => seg
    | Some((seg_l, match, seg_r)) =>
      let t = Tile.reassemble(match);
      let children = List.map(reassemble, t.children);
      let children = inner_regrout(children);
      let p =
        Tile.to_piece({
          ...t,
          children,
        });
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

let first_string =
  fun
  | [] => "EMPTY"
  | [Piece.Secondary(w), ..._] => Secondary.get_string(w.content)
  | [Piece.Projector(_), ..._] => "PROJECTOR"
  | [Piece.Grout(_), ..._] => "?"
  | [Piece.Tile(t), ..._] => t.label |> List.hd;

let last_string =
  fun
  | [] => "EMPTY"
  | xs =>
    switch (ListUtil.last(xs)) {
    | Piece.Secondary(w) => Secondary.get_string(w.content)
    | Piece.Grout(_) => "?"
    | Piece.Projector(_) => "PROJECTOR"
    | Piece.Tile(t) => t.label |> ListUtil.last
    };

let sort_of = (skel: Skel.t, seg: t): Sort.t =>
  Skel.root(skel) |> Aba.first_a |> List.nth(seg) |> Piece.sort |> fst;

let root_id = (skel: Skel.t, seg: t): Id.t =>
  skel |> Skel.root |> Util.Aba.first_a |> List.nth(seg) |> Piece.id;

let rec deep_tile_complete = (seg: t): bool =>
  List.for_all(
    t => Tile.is_complete(t) && List.for_all(deep_tile_complete, t.children),
    tiles(seg),
  );

let mk_duo = (sort: Sort.t, seg: t): Piece.t =>
  Piece.mk_tile(Form.mk_parens(sort), [seg]);

let parenthesize = (~sort: option(Sort.t)=?, seg: t): Piece.t => {
  /* If piece is anything other than a Tile, and override sort is not
   * specified, sort will be Any (see Piece.Sort) */
  let sort = Option.value(sort, ~default=sort_of(skel(seg), seg));
  mk_duo(sort, seg);
};

let unparenthesize = (seg: t): t =>
  switch (seg) {
  | [piece] => Piece.unparenthesize(piece)
  | _ => seg
  };

let rec take_while_secondary = (seg: t): (t, t) =>
  switch (seg) {
  | [] => ([], [])
  | [Piece.Secondary(_) as p, ...rest] =>
    let (taken, remaining) = take_while_secondary(rest);
    ([p, ...taken], remaining);
  | rest => ([], rest)
  };

let padding_of = (seg: t): (t, t) => {
  let (left, rest) = take_while_secondary(seg);
  let (right, _) = take_while_secondary(List.rev(rest));
  (left, List.rev(right));
};

let is_padded = (seg: t): bool =>
  switch (padding_of(seg)) {
  | ([], []) => false
  | _ => true
  };

module IDs = {
  /* Assign new ids for every piece in segment */
  let rec replace = (seg: t): t => List.map(replace_piece, seg)
  and replace_piece = (~id=?, p: Piece.t): Piece.t => {
    let id = Option.value(~default=Id.mk(), id);
    switch (p) {
    | Tile(t) =>
      Tile({
        ...t,
        children: List.map(replace, t.children),
        id,
      })
    | Grout(grout) =>
      Grout({
        ...grout,
        id,
      })
    | Secondary(w) =>
      Secondary({
        ...w,
        id,
      })
    | Projector(p) =>
      /* Need to keep projector and contained piece id in-sync */
      let id = Id.mk();
      let syntax = replace_piece(~id, p.syntax);
      Projector({
        ...p,
        syntax,
        id,
      });
    };
  };

  /* Get all piece ids in the segment */
  let rec all = (seg: t): list(Id.t) => List.concat_map(all_piece, seg)
  and all_piece = (p: Piece.t): list(Id.t) => {
    let id = Piece.id(p);
    switch (p) {
    | Tile(t) => [id, ...List.concat_map(all, t.children)]
    | Grout(_)
    | Secondary(_) => [id]
    | Projector(p) => [id, ...all_piece(p.syntax)]
    };
  };
};

let to_string = Base.segment_to_string;

/* Secondary collection for outer secondary model.
   Collects (before, after) secondary runs for each term based on skeleton structure. */
module SecondaryCollection = {
  type secondary_runs = Language.IdTagged.IdTag.secondary_runs;
  type secondary_map = Id.Map.t(secondary_runs);

  /* Collect secondary pieces immediately before index `idx` in segment */
  let collect_before = (seg: t, idx: int): list(Secondary.t) => {
    /* Walk backwards from idx-1, collecting consecutive secondary pieces */
    let rec go = (i, acc) =>
      if (i < 0) {
        acc;
      } else {
        switch (List.nth(seg, i)) {
        | Piece.Secondary(s) => go(i - 1, [s, ...acc])
        | _ => acc
        };
      };
    go(idx - 1, []);
  };

  /* Collect secondary pieces immediately after index `idx` in segment */
  let collect_after = (seg: t, idx: int): list(Secondary.t) => {
    let len = List.length(seg);
    let rec go = (i, acc) =>
      if (i >= len) {
        List.rev(acc);
      } else {
        switch (List.nth(seg, i)) {
        | Piece.Secondary(s) => go(i + 1, [s, ...acc])
        | _ => List.rev(acc)
        };
      };
    go(idx + 1, []);
  };

  /* Get the representative ID for a piece (used as the key in the map) */
  let piece_id = (seg: t, idx: int): option(Id.t) =>
    switch (List.nth_opt(seg, idx)) {
    | Some(Piece.Tile(t)) => Some(t.id)
    | Some(Piece.Projector(p)) => Some(p.id)
    | Some(Piece.Grout(g)) => Some(g.id)
    | _ => None
    };

  /* Collect secondary from a segment by directly walking through pieces.
     This handles segments with embedded Secondary pieces (like projector children)
     where skel() would throw Input_contains_secondary. */
  let rec collect_from_seg_direct =
          (seg: t, acc: secondary_map): secondary_map => {
    /* Find all non-secondary pieces and collect their before/after secondary */
    let rec find_pieces = (idx, pieces_acc) =>
      if (idx >= List.length(seg)) {
        List.rev(pieces_acc);
      } else {
        switch (List.nth(seg, idx)) {
        | Piece.Secondary(_) => find_pieces(idx + 1, pieces_acc)
        | _ =>
          find_pieces(idx + 1, [(idx, List.nth(seg, idx)), ...pieces_acc])
        };
      };

    let pieces = find_pieces(0, []);
    List.fold_left(
      (acc, (idx, piece)) => {
        let before = collect_before(seg, idx);
        let after = collect_after(seg, idx);
        switch (piece) {
        | Piece.Tile({id, children, _}) =>
          /* Add secondary for this tile and recurse into children */
          let acc = Id.Map.add(id, (before, after), acc);
          List.fold_left(
            (acc, child_seg) => collect_from_seg(child_seg, acc),
            acc,
            children,
          );
        | Piece.Projector({id, syntax, _}) =>
          /* Add secondary for projector and recurse into its content */
          let acc = Id.Map.add(id, (before, after), acc);
          let inner_seg = Piece.unparenthesize(syntax);
          collect_from_seg(inner_seg, acc);
        | Piece.Grout({id, _}) => Id.Map.add(id, (before, after), acc)
        | _ => acc
        };
      },
      acc,
      pieces,
    );
  }

  /* Recursively collect secondary from a segment (for compound tile children).
     Falls back to direct collection when the segment contains Secondary pieces. */
  and collect_from_seg = (child_seg: t, acc: secondary_map): secondary_map =>
    try(collect_from_skel(child_seg, skel(child_seg), acc)) {
    | Skel.Nonconvex_segment => acc
    | Skel.Input_contains_secondary =>
      /* Segment has embedded Secondary pieces, use direct collection */
      collect_from_seg_direct(child_seg, acc)
    }
  /* Recursively collect secondary for all terms in the skeleton.

     IMPORTANT: To avoid duplication, we use selective collection based on
     skeleton node type. The issue is that compound nodes (Bin, Pre, Post)
     have range boundaries that coincide with their children's boundaries.
     If both parent and child collect at the same position, we get duplicates.

     Solution - collect based on which boundaries are "owned" by this node:
     - Op (leaf): both before and after (no children to conflict with)
     - Bin: neither (before = left child's before, after = right child's after)
     - Pre: before only (after = operand's after)
     - Post: after only (before = operand's before)

     This ensures each piece of secondary is collected exactly once. */
  and collect_from_skel =
      (seg: t, skel: Skel.t, acc: secondary_map): secondary_map => {
    let (start_idx, end_idx) = Skel.range(skel);

    /* Determine which boundaries this node "owns" based on skeleton type */
    let (collect_before_boundary, collect_after_boundary) =
      switch (skel) {
      | Skel.Op(_) => (true, true) /* Leaves own both boundaries */
      | Skel.Pre(_, _) => (true, false) /* Pre owns before (operator), not after (operand) */
      | Skel.Post(_, _) => (false, true) /* Post owns after (operator), not before (operand) */
      | Skel.Bin(_, _, _) => (false, false) /* Bin owns neither (both are operands) */
      };

    let before =
      collect_before_boundary ? collect_before(seg, start_idx) : [];
    let after = collect_after_boundary ? collect_after(seg, end_idx) : [];

    /* Add secondary for the root pieces of this skeleton node */
    let root = Skel.root(skel);
    let root_indices = Aba.get_as(root);
    let acc =
      switch (List.nth_opt(root_indices, 0)) {
      | Some(first_idx) =>
        switch (piece_id(seg, first_idx)) {
        | Some(id) => Id.Map.add(id, (before, after), acc)
        | None => acc
        }
      | None => acc
      };

    /* Process children segments of compound tiles.
       For tiles like Let, Fun, etc., the children are in separate segments
       inside the tile's `children` field. We need to collect secondary from
       those segments too. */
    let acc =
      List.fold_left(
        (acc, idx) =>
          switch (List.nth_opt(seg, idx)) {
          | Some(Piece.Tile({children, _})) =>
            List.fold_left(
              (acc, child_seg) => collect_from_seg(child_seg, acc),
              acc,
              children,
            )
          | Some(Piece.Projector({syntax, _})) =>
            /* Projectors wrap their content in parentheses. Extract the inner
               segment and recursively collect secondary from it. */
            let inner_seg = Piece.unparenthesize(syntax);
            collect_from_seg(inner_seg, acc);
          | _ => acc
          },
        acc,
        root_indices,
      );

    /* Process children of this skeleton node (operands between operators) */
    let children = Aba.get_bs(root);
    let acc =
      List.fold_left(
        (acc, child) => collect_from_skel(seg, child, acc),
        acc,
        children,
      );

    /* Process left/right sub-skeletons for Pre/Post/Bin */
    switch (skel) {
    | Skel.Op(_) => acc
    | Skel.Pre(_, right) => collect_from_skel(seg, right, acc)
    | Skel.Post(left, _) => collect_from_skel(seg, left, acc)
    | Skel.Bin(left, _, right) =>
      let acc = collect_from_skel(seg, left, acc);
      collect_from_skel(seg, right, acc);
    };
  };

  /* Main entry point: collect outer secondary for all terms in segment */
  let collect = (seg: t): secondary_map =>
    try(collect_from_skel(seg, skel(seg), Id.Map.empty)) {
    | Skel.Nonconvex_segment
    | Skel.Input_contains_secondary => Id.Map.empty
    };
};
