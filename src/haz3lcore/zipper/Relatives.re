open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  siblings: Siblings.t,
  ancestors: Ancestors.t,
};

let empty = {
  siblings: Siblings.empty,
  ancestors: Ancestors.empty,
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

/* Walk a segment in direction [d], looking for the first Splice piece
 * (in document order along [d]) for which [pred] returns true. Recurses
 * into Tile children, but does NOT recurse into nested Projector pieces.
 *
 * If found, returns:
 *   - [ancs]: ancestor frames to push onto the stack (innermost first),
 *     consisting of one Splice frame plus zero or more Tile frames
 *     describing the path from the outermost containing piece in [seg]
 *     down to the matched splice. The OUTERMOST frame's gen_sibs is
 *     stubbed to (Segment.empty, Segment.empty); the caller is expected
 *     to use the returned [outer_left]/[outer_right] for the surrounding
 *     Projector ancestor's [before]/[after] fields.
 *   - [splice_sibs]: siblings to seed inside the splice, with the caret
 *     at the entry edge for direction [d] (left edge for Right, right
 *     edge for Left).
 *   - [splice]: the matched splice piece.
 *   - [outer_left]/[outer_right]: pieces in [seg] adjacent to the
 *     outermost descent piece (in natural left-to-right order).
 *
 * Stateful predicates are supported: [pred] is invoked on splices in
 * document order along [d], including for splices that don't end up
 * being chosen (e.g. ones inside tile children we end up not descending
 * through because no match was found). */
let rec find_splice_descent_when =
        (
          ~sort: Sort.t=Sort.Any,
          d: Direction.t,
          pred: Base.splice => bool,
          seg: Segment.t,
        )
        : option(
            (Ancestors.t, Siblings.t, Base.splice, Segment.t, Segment.t),
          ) => {
  let pieces_in_d_order =
    switch (d) {
    | Right => seg
    | Left => List.rev(seg)
    };
  let split_at_current = (acc: list(Piece.t), rest: list(Piece.t)) =>
    /* [acc] holds pieces visited so far in scan order (newest first);
     * [rest] holds the remaining pieces in scan order. Compute the
     * (left, right) partition in seg's natural order. */
    switch (d) {
    | Right => (List.rev(acc), rest)
    | Left => (List.rev(rest), acc)
    };
  let splice_inner_sibs = (s: Base.splice): Siblings.t =>
    switch (d) {
    | Right => (Segment.empty, s.content)
    | Left => (s.content, Segment.empty)
    };
  let rec scan = (acc: list(Piece.t), rest: list(Piece.t)) =>
    switch (rest) {
    | [] => None
    | [Base.Splice(s) as p, ...rest_pieces] =>
      if (pred(s)) {
        let (before, after) = split_at_current(acc, rest_pieces);
        let splice_anc: Ancestor.splice_anc = {
          id: s.id,
          sort,
        };
        let ancs = [
          (Ancestor.Splice(splice_anc), (Segment.empty, Segment.empty)),
        ];
        Some((ancs, splice_inner_sibs(s), s, before, after));
      } else {
        scan([p, ...acc], rest_pieces);
      }
    | [Base.Tile(t) as p, ...rest_pieces] =>
      let n_kids = List.length(t.children);
      let kid_indices =
        switch (d) {
        | Right => List.init(n_kids, i => i)
        | Left => List.init(n_kids, i => n_kids - 1 - i)
        };
      let rec try_kids = idx_list =>
        switch (idx_list) {
        | [] => None
        | [i, ...rest_idx] =>
          let child = List.nth(t.children, i);
          let child_sort =
            i < List.length(t.mold.in_) ? List.nth(t.mold.in_, i) : Sort.Any;
          switch (find_splice_descent_when(~sort=child_sort, d, pred, child)) {
          | None => try_kids(rest_idx)
          | Some((sub_ancs, sub_sibs, sub_splice, child_l, child_r)) =>
            let (shards_l, shards_r) = ListUtil.split_n(i + 1, t.shards);
            let (kids_l, kids_after_active) =
              ListUtil.split_n(i, t.children);
            let kids_r =
              switch (kids_after_active) {
              | [] => []
              | [_active, ...rest_kids] => rest_kids
              };
            let tile_anc: Ancestor.tile_anc = {
              id: t.id,
              label: t.label,
              mold: t.mold,
              shards: (shards_l, shards_r),
              children: (kids_l, kids_r),
            };
            let (before, after) = split_at_current(acc, rest_pieces);
            /* Promote the recursive call's outermost frame to an
             * "interior" frame: assign it the proper gen_sibs (the
             * pieces in this tile's active child segment around the
             * descent path). Then add this tile's frame on top of it,
             * with stubbed gen_sibs of (empty, empty); our caller will
             * fix our outermost frame's gen_sibs in turn. */
            let sub_ancs_fixed =
              switch (List.rev(sub_ancs)) {
              | [] => sub_ancs
              | [(outermost_anc, _), ...rest_rev] =>
                List.rev([(outermost_anc, (child_l, child_r)), ...rest_rev])
              };
            let tile_frame = (
              Ancestor.Tile(tile_anc),
              (Segment.empty, Segment.empty),
            );
            let ancs = sub_ancs_fixed @ [tile_frame];
            Some((ancs, sub_sibs, sub_splice, before, after));
          };
        };
      switch (try_kids(kid_indices)) {
      | Some(_) as r => r
      | None => scan([p, ...acc], rest_pieces)
      };
    | [p, ...rest_pieces] => scan([p, ...acc], rest_pieces)
    };
  scan([], pieces_in_d_order);
};

/* Find the first splice in [seg] in direction [d] (recursing into
 * Tile children). Convenience wrapper around [find_splice_descent_when]
 * with a constant-true predicate. */
let find_splice_descent =
    (d: Direction.t, seg: Segment.t)
    : option((Ancestors.t, Siblings.t, Base.splice, Segment.t, Segment.t)) =>
  find_splice_descent_when(d, _ => true, seg);

/* Enter a projector from outside: pushes a Projector frame plus zero or
 * more Tile frames plus a Splice frame onto the ancestors stack, and
 * seeds the caret inside the leftmost splice (if moving right) or
 * rightmost splice (if moving left). Splices nested arbitrarily deep
 * inside tile children of [pr.syntax] are supported. Returns None if
 * the projector has no splices — in that case the caller should fall
 * back to treating the projector as an ordinary piece. */
let enter_projector =
    (
      d: Direction.t,
      pr: Base.projector,
      outer_sibs: Siblings.t,
      ancestors: Ancestors.t,
    )
    : option(t) =>
  switch (find_splice_descent(d, pr.syntax)) {
  | None => None
  | Some((descent_ancs, splice_sibs, _splice, outer_left, outer_right)) =>
    let proj_anc: Ancestor.proj_anc = {
      id: pr.id,
      kind: pr.kind,
      model: pr.model,
      before: outer_left,
      after: outer_right,
    };
    Some({
      siblings: splice_sibs,
      ancestors:
        descent_ancs
        @ [(Ancestor.Projector(proj_anc), outer_sibs), ...ancestors],
    });
  };

let enter_splice =
    (
      d: Direction.t,
      sp: Base.splice,
      ~sort: Sort.t=Sort.Any,
      outer_sibs: Siblings.t,
      ancestors: Ancestors.t,
    )
    : t => {
  let splice_anc: Ancestor.splice_anc = {
    id: sp.id,
    sort,
  };
  let splice_inner_sibs: Siblings.t =
    switch (d) {
    | Right => (Segment.empty, sp.content)
    | Left => (sp.content, Segment.empty)
    };
  {
    siblings: splice_inner_sibs,
    ancestors: [(Ancestor.Splice(splice_anc), outer_sibs), ...ancestors],
  };
};

let exit_splice = (d: Direction.t, rs: t): option(t) => {
  switch (rs.ancestors) {
  | [(Ancestor.Splice(sp), splice_gen_sibs), ...rest_ancs] =>
    let zipped_splice =
      Piece.mk_splice(~id=sp.id, fst(rs.siblings) @ snd(rs.siblings));
    let splice_active_seg =
      fst(splice_gen_sibs) @ [zipped_splice, ...snd(splice_gen_sibs)];
    let rec walk_to_projector = (active_seg: Segment.t, ancs: Ancestors.t) =>
      switch (ancs) {
      | [(Ancestor.Tile(tile_anc), tile_gen_sibs), ...rest] =>
        let zipped_tile = Ancestor.zip(active_seg, Ancestor.Tile(tile_anc));
        let new_seg =
          fst(tile_gen_sibs) @ [zipped_tile, ...snd(tile_gen_sibs)];
        walk_to_projector(new_seg, rest);
      | [(Ancestor.Projector(pr_anc), outer_sibs), ...rest] =>
        Some((active_seg, pr_anc, outer_sibs, rest))
      | _ => None
      };
    switch (walk_to_projector(splice_active_seg, rest_ancs)) {
    | None => None
    | Some((pr_active_subtree, pr_anc, outer_sibs, after_proj)) =>
      let pr_full_syntax = pr_anc.before @ pr_active_subtree @ pr_anc.after;
      let passed = ref(false);
      let pred = (s: Base.splice) =>
        if (passed^) {
          true;
        } else if (Id.equal(s.id, sp.id)) {
          passed := true;
          false;
        } else {
          false;
        };
      switch (find_splice_descent_when(d, pred, pr_full_syntax)) {
      | Some((descent_ancs, splice_sibs, _next, outer_l, outer_r)) =>
        let new_pr_anc: Ancestor.proj_anc = {
          ...pr_anc,
          before: outer_l,
          after: outer_r,
        };
        Some({
          siblings: splice_sibs,
          ancestors:
            descent_ancs
            @ [(Ancestor.Projector(new_pr_anc), outer_sibs), ...after_proj],
        });
      | None =>
        let projector_piece =
          Base.Projector(
            ProjectorCore.mk(
              ~id=pr_anc.id,
              pr_anc.kind,
              pr_full_syntax,
              pr_anc.model,
            ),
          );
        Some({
          siblings:
            Siblings.push(Direction.toggle(d), projector_piece, outer_sibs),
          ancestors: after_proj,
        });
      };
    };
  | _ => None
  };
};

/* Core pop: attempts to take a piece in direction [d] from siblings; if
 * siblings are exhausted, pops one ordinary tile ancestor frame and retries.
 * Projector/splice entry and exit are handled explicitly by Move.re; keeping
 * them out of core pop prevents hidden movement that also consumes an inner
 * token. */
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
    | [(Ancestor.Splice(_) | Projector(_), _), ..._] => None
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
  |> Option.map(p => Ancestor.zip(l_sibs @ sel @ r_sibs, p));

let delete_parent = ({siblings, ancestors}: t): t => {
  switch (ancestors) {
  | [] => {
      siblings,
      ancestors,
    }
  | [(_, p_sibs), ...ancestors] => {
      siblings: Siblings.concat([siblings, p_sibs]),
      ancestors,
    }
  };
};

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
  | [(Ancestor.Tile(a), sibs), ...rest] =>
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
    | Some([_]) => [(Ancestor.Tile(a), sibs), ...rest]
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
      [(Ancestor.Tile(a), sibs), ...rest];
    };
  | [hd, ...rest] => [hd, ...rest]
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
      switch (ListUtil.split_last_opt(gs_l), gs_r) {
      | (Some((_, g_l)), [g_r, ..._]) =>
        Grout.fits(g_l, g_r)
          // note: assumes single grout invariant in un-caret-interrupted trim
          ? (ws(trim_l), ws(trim_r))  //(ws(trim_l), seg_r)
          : (
            switch (d) {
            | Left => (ws(trim_l), seg_r)
            | Right => (seg_l, ws(trim_r))
            }
          )
      | (Some((_, g)), []) =>
        Grout.fits_shape(g, s_r) ? (seg_l, seg_r) : (ws(trim_l), seg_r)
      | (None, [g, ..._]) =>
        Grout.fits_shape(g, s_l) ? (seg_l, seg_r) : (seg_l, ws(trim_r))
      | (None, []) =>
        Nib.Shape.fits(s_l, s_r)
          ? (seg_l, seg_r)
          // can modulate with directional arg
          : (
            switch (d) {
            | Left =>
              let trim = add_grout(s_r, trim_r);
              (seg_l, to_seg(trim));
            | Right =>
              let trim = add_grout(s_l, trim_l);
              (to_seg(trim), seg_r);
            }
          )
      };
    };
    (pre @ trim_l, trim_r @ suf);
  };
  let siblings =
    switch (ancestors) {
    | [(Ancestor.Splice(_), _), ..._] =>
      /* Inside splice editors, the content's outer shape should be
       * Convex/Convex (matching the surrounding Splice piece's nibs).
       * The (l, r) passed to [regrout] is the *environment* shape, so
       * we pass (Concave, Concave) — meaning the splice's inside acts
       * like the inside of a Convex/Convex tile, just as tile children
       * are regrouted with (concave, concave). */
      let pre_len = List.length(fst(siblings));
      siblings
      |> Siblings.zip
      |> Segment.regrout(Nib.Shape.(concave(), concave()))
      |> Siblings.unzip(pre_len);
    | _ => siblings
    };
  {
    siblings,
    ancestors,
  };
};

let reassemble_parent = (rs: t): t =>
  switch (rs.ancestors) {
  | [] => rs
  | [(Ancestor.Projector(_) | Splice(_), _), ..._] =>
    /* Projector and Splice ancestors have no shards to reassemble. */
    rs
  | [(Tile(a), sibs), ...ancs] =>
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
        let a: Ancestor.tile_anc = {
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
        let a: Ancestor.tile_anc = {
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
      ancestors: [(Ancestor.Tile(a), sibs), ...ancs],
    };
  };

let reassemble_siblings = (rs: t) => {
  ...rs,
  siblings: Siblings.reassemble(rs.siblings),
};

/* Rescan across combined siblings: converts standalone monotiles
 * that match missing shards of incomplete tiles on the other side.
 * This must run before the cross-sibling `go` function so that
 * converted tiles have the correct IDs for reassembly. */
let rescan_siblings = (rs: t) => {
  ...rs,
  siblings: Siblings.rescan(rs.siblings),
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
