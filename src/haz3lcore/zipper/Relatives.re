open Util_web;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  siblings: Siblings.t,
  ancestors: Ancestors.t,
};

let push = (d: Direction.t, p: Piece.t, rs: t): t => {
  ...rs,
  siblings: Siblings.push(d, p, rs.siblings),
};

let prepend = (d: Direction.t, seg: Segment.t, rs: t): t => {
  let siblings = Siblings.prepend(d, seg, rs.siblings);
  {
    ...rs,
    siblings,
  };
};

let pop = (d: Direction.t, rs: t): option((Piece.t, t)) =>
  switch (Siblings.pop(d, rs.siblings)) {
  | Some((p, siblings)) =>
    Some((
      p,
      {
        ...rs,
        siblings,
      },
    ))
  | None =>
    switch (rs.ancestors) {
    | [] => None
    | [(ancestor, siblings), ...ancestors] =>
      open OptUtil.Syntax;
      let siblings' = Ancestor.disassemble(ancestor);
      let+ (p, siblings) =
        Siblings.(pop(d, concat([rs.siblings, siblings', siblings])));
      (
        p,
        {
          siblings,
          ancestors,
        },
      );
    }
  };

let zip = (~sel=Segment.empty, {siblings, ancestors}: t) =>
  Ancestors.zip(Siblings.zip(~sel, siblings), ancestors);

let local_missing_shards = ({siblings, ancestors}: t): list(Tile.t) => {
  Siblings.local_missing_shards(siblings)
  @ Ancestors.local_missing_shards(ancestors);
};

let parent =
    (~sel=Segment.empty, {siblings: (l_sibs, r_sibs), ancestors}: t)
    : option(Piece.t) =>
  ancestors
  |> Ancestors.parent
  |> Option.map(p => Base.Tile(Ancestor.zip(l_sibs @ sel @ r_sibs, p)));

/* The sort at the current insertion point, accounting for
 * infix operators with heterogeneous child sorts (e.g. type
 * annotation ':' in patterns). This looks at the right nib
 * of the last tile to the left, which determines what sort
 * should come next - the same logic used by Segment.remold. */
let sort = (~root, {siblings: (pre, _), ancestors}: t): Sort.t => {
  let outer_sort = Ancestors.sort(root, ancestors);
  let rec find_last_tile =
    fun
    | [] => None
    | [p, ...rest] =>
      switch (Piece.is_tile(p)) {
      | Some(t) => Some(t)
      | None => find_last_tile(rest)
      };
  switch (find_last_tile(List.rev(pre))) {
  | None => outer_sort
  | Some(t) =>
    let (_, r) = Tile.nibs(t);
    r.sort;
  };
};

/* Remold the immediate parent ancestor tile based on its
 * sibling context. This handles cases where completing a
 * bidelimited form (e.g. putting down `(` from backpack to
 * complete `(...)`) leaves the caret inside, and the parent
 * tile needs a different mold (e.g. `ap(...)` instead of
 * plain parens) to fit its neighbors. */
let remold_parent = (~root, ancestors: Ancestors.t): Ancestors.t =>
  switch (ancestors) {
  | [] => []
  | [(a, sibs), ...rest] =>
    let outer_sort = Ancestors.sort(root, rest);
    let (pre, _) = sibs;
    let sort = {
      let rec find_last_tile = (
        fun
        | [] => None
        | [p, ...rest] =>
          switch (Piece.is_tile(p)) {
          | Some(t) => Some(t)
          | None => find_last_tile(rest)
          }
      );
      switch (find_last_tile(List.rev(pre))) {
      | None => outer_sort
      | Some(t) =>
        let (_, r) = Tile.nibs(t);
        r.sort;
      };
    };
    switch (Form.Molds.try_get(sort, a.label)) {
    | None
    | Some([_]) => [(a, sibs), ...rest]
    | Some(molds) =>
      let (pre, _) = sibs;
      let (_, left_shape, _) =
        Segment.shape_affix(Left, pre, Nib.Shape.concave());
      let l_idx = Ancestor.l_shard(a);
      let a =
        switch (
          molds
          |> List.filter(mold => {
               let (l_nib, _) = Mold.nibs(~index=l_idx, mold);
               Nib.Shape.fits(left_shape, Nib.shape(l_nib));
             })
        ) {
        | [mold, ..._] => {
            ...a,
            mold,
          }
        | [] => a
        };
      [(a, sibs), ...rest];
    };
  };

let remold = ({siblings, ancestors}: t, root: Sort.t): t => {
  let s = Ancestors.sort(root, ancestors);
  let siblings = Siblings.remold(siblings, s);
  let ancestors = remold_parent(~root, ancestors);
  {
    ancestors,
    siblings,
  };
};

let regrout = (d: Direction.t, {siblings, ancestors}: t): t => {
  /* Direction is side of grout caret will end up on */

  let ancestors = Ancestors.regrout(ancestors);
  let siblings = {
    let ((pre, s_l, trim_l), (trim_r, s_r, suf)) =
      Siblings.regrout(siblings);
    let (trim_l, trim_r) = {
      open Segment.Trim;
      let ((_, gs_l), (_, gs_r)) = (trim_l, trim_r);
      let (seg_l, seg_r) = (to_seg(trim_l), to_seg(trim_r));
      /* Same junction principle as Trim.regrout, straddling the caret:
       * if the neighboring shapes fit each other no grout belongs here,
       * else exactly one grout of the complementary shape does. Judging
       * kept grout against BOTH shapes matters because remold can change
       * a neighbor out from under a previously-fitting grout (#2446:
       * completing `use _ in` remolds a following infix `-` to prefix,
       * whose convex nib no longer admits the convex grout). */
      if (Nib.Shape.fits(s_l, s_r)) {
        switch (gs_l, gs_r) {
        | ([], []) => (seg_l, seg_r)
        | _ => (ws(trim_l), ws(trim_r))
        };
      } else {
        /* s_l and s_r are same-class here, so a grout fitting one fits
         * both, and at most one shape of grout fits. */
        let fits = g => Grout.fits_shape(g, s_l);
        let g_l = Option.map(snd, ListUtil.split_last_opt(gs_l));
        let g_r = ListUtil.hd_opt(gs_r);
        switch (g_l, g_r) {
        | (Some(gl), Some(gr)) when fits(gl) && fits(gr) =>
          // note: assumes single grout invariant in un-caret-interrupted trim
          switch (d) {
          | Left => (ws(trim_l), seg_r)
          | Right => (seg_l, ws(trim_r))
          }
        | (Some(gl), _) when fits(gl) => (seg_l, ws(trim_r))
        | (_, Some(gr)) when fits(gr) => (ws(trim_l), seg_r)
        | _ =>
          // no fitting grout present: mint one on the caret's side
          switch (d) {
          | Left =>
            let trim = add_grout(s_r, strip_grout(trim_r));
            (ws(trim_l), to_seg(trim));
          | Right =>
            let trim = add_grout(s_l, strip_grout(trim_l));
            (to_seg(trim), ws(trim_r));
          }
        };
      };
    };
    (pre @ trim_l, trim_r @ suf);
  };
  {
    siblings,
    ancestors,
  };
};

let reassemble_parent = (rs: t): t =>
  switch (rs.ancestors) {
  | [] => rs
  | [(a, sibs), ...ancs] =>
    let (l, r) =
      rs.siblings
      |> Siblings.split_by_matching(a.id)
      |> TupleUtil.map2(Aba.trim);
    let flatten_match =
      Aba.fold_right(
        (t: Tile.t, kid, (shards, kids)) =>
          Aba.mk(t.shards @ shards, t.children @ [kid, ...kids]),
        (t: Tile.t) => Aba.mk(t.shards, t.children),
      );
    let (a, l) =
      switch (l) {
      | None => (a, fst(rs.siblings))
      | Some((outer_l, match_l, inner_l)) =>
        let (shards_l, kids_l) = flatten_match(match_l);
        let a = {
          ...a,
          shards: a.shards |> PairUtil.map_fst(ss => ss @ shards_l),
          children:
            a.children
            |> PairUtil.map_fst(kids =>
                 Segment.inner_regrout(kids @ [outer_l, ...kids_l])
               ),
        };
        (a, inner_l);
      };
    let (a, r) =
      switch (r) {
      | None => (a, snd(rs.siblings))
      | Some((inner_r, match_r, outer_r)) =>
        let (shards_r, kids_r) = flatten_match(match_r);
        let a = {
          ...a,
          shards: a.shards |> PairUtil.map_snd(ss => shards_r @ ss),
          children:
            a.children
            |> PairUtil.map_snd(kids =>
                 Segment.inner_regrout([outer_r, ...kids_r] @ kids)
               ),
        };
        (a, inner_r);
      };
    {
      siblings: (l, r),
      ancestors: [(a, sibs), ...ancs],
    };
  };

let reassemble_siblings = (rs: t) => {
  ...rs,
  siblings: Siblings.reassemble(rs.siblings),
};

let reassemble = (rs: t): t => {
  let rec go = (rs: t): t =>
    switch (Segment.incomplete_tiles(snd(rs.siblings))) {
    | [] => rs
    | [t, ..._] =>
      switch (
        rs.siblings
        |> Siblings.split_by_matching(t.id)
        |> TupleUtil.map2(Aba.trim)
      ) {
      | (_, None) => failwith("impossible")
      | (None, Some((inner_r, match_r, outer_r))) =>
        let {siblings: (pre, suf), ancestors} =
          go({
            ...rs,
            siblings: (fst(rs.siblings), outer_r),
          });
        let t = Tile.reassemble(match_r);
        let suf = Segment.concat([inner_r, [Tile.to_piece(t), ...suf]]);
        {
          siblings: (pre, suf),
          ancestors,
        };
      | (
          Some((outer_l, match_l, inner_l)),
          Some((inner_r, match_r, outer_r)),
        ) =>
        let rs =
          go({
            ...rs,
            siblings: (outer_l, outer_r),
          });
        let ancestors = [
          (Ancestor.reassemble(match_l, match_r), rs.siblings),
          ...rs.ancestors,
        ];
        let siblings = (inner_l, inner_r);
        {
          ancestors,
          siblings,
        };
      }
    };
  rs |> reassemble_siblings |> reassemble_parent |> go;
};
