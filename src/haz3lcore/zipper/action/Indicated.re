open Util;

[@deriving show]
type relation =
  | Parent
  | Sibling;

type piece = {
  piece: Piece.t,
  side: Direction.t,
  relation,
};

/* Tiles with a Concave left nib get their left position as a special
   case under inward bias. Without this, they could never be indicated
   from their left side, since the left neighbor's Convex right nib
   would always win. This covers infix operators (Concave, Concave)
   and application forms like Ap parens (Concave, Convex). */
let has_concave_left_nib = (p: Piece.t): bool =>
  switch (p) {
  | Tile({mold: {nibs: ({shape: Concave(_), _}, _), _}, _}) => true
  | _ => false
  };

/* The caret-facing nib shape of a piece: right nib for L, left nib for R */
let caret_facing_shape = (d: Direction.t, p: Piece.t): option(Nib.Shape.t) =>
  switch (Piece.shapes(p)) {
  | Some((l, r)) =>
    switch (d) {
    | Left => Some(r) /* L piece: its right nib faces the caret */
    | Right => Some(l) /* R piece: its left nib faces the caret */
    }
  | None => None
  };

/* Core indication logic. Determines the indicated piece at the
   current caret position using INWARD bias: when between two pieces,
   favor the one whose caret-facing nib is Convex (term-shaped).
   Infix operators get their left (designated) position as a special case.

   Parameters:
   - no_ws: if true, return None when only secondary neighbors exist;
     if false, return the secondary piece (for callers that always
     need an answer, like index/direction queries).
   - ign: predicate for pieces to skip (typically is_secondary) */
let indicated =
    (~no_ws: bool, ~ign: Piece.t => bool, z: ZipperBase.t): option(piece) => {
  switch (
    Siblings.neighbors(ZipperBase.sibs_with_sel(z)),
    ZipperBase.parent(z),
  ) {
  /* Empty syntax => no indication */
  | ((None, None), None) => None
  /* L not secondary, R is secondary => indicate L */
  | ((Some(l), Some(r)), _) when !ign(l) && ign(r) =>
    Some({
      piece: l,
      side: Left,
      relation: Sibling,
    })
  /* L and R are secondarys => no indication */
  | ((Some(l), Some(r)), _) when ign(l) && ign(r) =>
    no_ws
      ? None
      : Some({
          piece: l,
          side: Left,
          relation: Sibling,
        })
  /* At right end of syntax and L is secondary => no indication */
  | ((Some(l), None), None) when ign(l) =>
    no_ws
      ? None
      : Some({
          piece: l,
          side: Left,
          relation: Sibling,
        })
  /* At left end of syntax and R is secondary => no indication */
  | ((None, Some(r)), None) when ign(r) =>
    no_ws
      ? None
      : Some({
          piece: r,
          side: Right,
          relation: Sibling,
        })
  /* No L and R is a secondary and there is a P => indicate P */
  | ((None, Some(r)), Some(parent)) when ign(r) =>
    Some({
      piece: parent,
      side: Left,
      relation: Parent,
    })
  /* Both L and R non-ignored, caret outer: inward bias with concave-left special case */
  | ((Some(l), Some(r)), _parent)
      when !ign(l) && !ign(r) && z.caret == Outer =>
    if (has_concave_left_nib(r)) {
      /* R has concave left nib: this is R's designated position */
      Some({
        piece: r,
        side: Right,
        relation: Sibling,
      });
    } else {
      switch (caret_facing_shape(Left, l), caret_facing_shape(Right, r)) {
      /* R is convex (inward) => indicate R */
      | (_, Some(Convex)) =>
        Some({
          piece: r,
          side: Right,
          relation: Sibling,
        })
      /* L is convex (inward), R is not => indicate L */
      | (Some(Convex), _) =>
        Some({
          piece: l,
          side: Left,
          relation: Sibling,
        })
      /* Both concave or unknown => indicate L (fallback) */
      | _ =>
        Some({
          piece: l,
          side: Left,
          relation: Sibling,
        })
      };
    }
  /* L non-ignored, R ignored or absent, caret outer => indicate L.
   * L is the only meaningful piece, so indicate it regardless of shape. */
  | ((Some(l), _), _) when !ign(l) && z.caret == Outer =>
    Some({
      piece: l,
      side: Left,
      relation: Sibling,
    })
  /* No L, R non-ignored, parent exists, caret outer: inward bias
   * prefers the child (R) over the parent delimiter */
  | ((None, Some(r)), Some(_parent)) when !ign(r) && z.caret == Outer =>
    Some({
      piece: r,
      side: Right,
      relation: Sibling,
    })
  /* No L, R ignored or absent, some P, caret outer => indicate P */
  | ((None, _), Some(parent)) when z.caret == Outer =>
    Some({
      piece: parent,
      side: Left,
      relation: Parent,
    })
  /* R is not secondary, either no L or L is secondary or caret is inner => indicate R */
  | ((_, Some(r)), _) =>
    Some({
      piece: r,
      side: Right,
      relation: Sibling,
    })
  /* No R and there is a P => indicate P */
  | ((_, None), Some(parent)) =>
    Some({
      piece: parent,
      side: Right,
      relation: Parent,
    })
  /* There is an L but no R and no P => indicate L */
  //WEIRD: Right below seems wrong but behaves right
  | ((Some(l), None), None) =>
    Some({
      piece: l,
      side: Right,
      relation: Sibling,
    })
  };
};

/* For visual decoration (caret side, arms, projector/refractor highlighting).
   Ignores secondary and hole tiles. Used by CaretDec, Arms, CodeEditable, Backpack. */
let for_decoration =
  indicated(~no_ws=true, ~ign=p => Piece.(is_secondary(p) || is_hole(p)));

/* For identity/direction queries that always need an answer, even in
   whitespace. Ignores secondary but returns them as fallback. */
let for_index = indicated(~no_ws=false, ~ign=Piece.is_secondary);

let shard_index = (z: ZipperBase.t): option(int) =>
  switch (for_decoration(z)) {
  | None => None
  | Some({piece: p, side, relation}) =>
    switch (relation) {
    | Parent =>
      switch (Ancestors.parent(z.relatives.ancestors)) {
      | None => failwith("indicated_shard_index impossible")
      | Some({children: (before, _), _}) =>
        let before = List.length(before);
        switch (Siblings.neighbors(z.relatives.siblings)) {
        | (_, None) => Some(before + 1)
        | _ => Some(before)
        };
      }
    | Sibling =>
      switch (p) {
      | Secondary(_)
      | Projector(_) => Some(0)
      | Tile(t) =>
        switch (side) {
        | Left => Some(List.length(t.children))
        | Right => Some(0)
        }
      }
    }
  };

let direction = (z: ZipperBase.t): option(Direction.t) =>
  switch (for_index(z)) {
  | None => None
  | Some({side, _}) => Some(side)
  };

let index = (z: ZipperBase.t): option(Id.t) =>
  switch (for_index(z)) {
  | None => None
  | Some({piece, _}) => Some(Piece.id(piece))
  };

let piece'' = indicated(~no_ws=true, ~ign=Piece.is_secondary);

/* Search the info_map for an EmptyHole entry whose first ancestor
   matches the given parent_id. Used at virtual hole positions where
   the hole tile ID is transient and only exists in the info_map. */
let find_virtual_hole_ci =
    (parent_id: Id.t, info_map: Language.Statics.Map.t)
    : option(Language.Statics.Info.t) =>
  Id.Map.fold(
    (_, ci, acc) =>
      switch (acc) {
      | Some(_) => acc
      | None =>
        switch (ci) {
        | Language.Statics.Info.InfoExp({
            cls: Exp(EmptyHole),
            ancestors: [pid, ..._],
            _,
          })
            when pid == parent_id =>
          Some(ci)
        | Language.Statics.Info.InfoPat({
            cls: Pat(EmptyHole),
            ancestors: [pid, ..._],
            _,
          })
            when pid == parent_id =>
          Some(ci)
        | _ => None
        }
      },
    info_map,
    None,
  );

/* Check if the cursor is at a virtual hole by detecting a shape
   conflict at the current position */
let at_virtual_hole = (z: ZipperBase.t): bool => {
  let (l, r) = Siblings.shapes(ZipperBase.sibs_with_sel(z));
  !Nib.Shape.fits(l, r);
};

/* Try to find the virtual hole's EmptyHole entry by searching with
   multiple candidate parent IDs. At a virtual hole, the EmptyHole in
   the info_map has its first ancestor set to the containing tile's ID.
   We try: same-tile neighbors, left neighbor, right neighbor, ancestor. */
let try_find_virtual_hole_ci =
    (z: ZipperBase.t, info_map: Language.Statics.Map.t)
    : option(Language.Statics.Info.t) => {
  let try_id = id => find_virtual_hole_ci(id, info_map);
  let sibs = ZipperBase.sibs_with_sel(z);
  let (l_opt, r_opt) = Siblings.neighbors(Siblings.trim_secondary(sibs));
  /* First: check if neighbors are shards of the same tile */
  switch (l_opt, r_opt) {
  | (Some(Tile(tl)), Some(Tile(tr))) when tl.id == tr.id => try_id(tl.id)
  | _ =>
    /* Try left neighbor's ID (e.g., comma tile in `1, _`) */
    let result =
      switch (l_opt) {
      | Some(p) => try_id(Piece.id(p))
      | None => None
      };
    switch (result) {
    | Some(_) => result
    | None =>
      /* Try right neighbor's ID */
      let result =
        switch (r_opt) {
        | Some(p) => try_id(Piece.id(p))
        | None => None
        };
      switch (result) {
      | Some(_) => result
      | None =>
        /* Try ancestor */
        switch (z.relatives.ancestors) {
        | [(parent, _), ..._] => try_id(parent.id)
        | [] => None
        }
      };
    };
  };
};

let secondary_fallback =
    (z: ZipperBase.t, info_map: Language.Statics.Map.t)
    : option(Language.Statics.Info.t) => {
  let sibs = ZipperBase.sibs_with_sel(z);
  let cls =
    switch (Siblings.neighbors(sibs)) {
    /* If on side of comment, say we're on comment */
    | (Some(Secondary(sl)), Some(Secondary(_)))
        when Secondary.is_comment(sl) =>
      Language.Secondary.cls_of(sl)
    | (Some(Secondary(_)), Some(Secondary(sr)))
        when Secondary.is_comment(sr) =>
      Language.Secondary.cls_of(sr)
    | (_, Some(Secondary(s)))
    | (Some(Secondary(s)), _) => Language.Secondary.cls_of(s)
    | _ => Language.Secondary.Whitespace
    };
  /* Derive sort by walking the ancestor stack (more accurate than
     borrowing from a neighbor piece, whose sort reflects the proxy's
     own position rather than the cursor's). Required for correct
     module-hole sort detection (CI.ModuleHoleSort tests).

     The ~root parameter is only consulted when the ancestor stack is
     empty — i.e., cursor on whitespace at the very top level of a
     non-Exp-rooted editor (Pat/Typ/Drv projectors). In that narrow
     case Sort.Exp is reported instead of the editor's actual root,
     affecting display only (cursor inspector, var-highlight tooltip,
     explain-this routing). All standard expression editors are
     unaffected. Threading ~root properly would require touching ~20
     files (Move, ProbePerform, ContextMenu, VarHighlight,
     HighLevelNodeMap and their callers) — deferred to a focused
     refactor. */
  let sort = Relatives.sort(~root=Sort.Exp, z.relatives);
  /* Try to find a non-secondary proxy for context */
  let ctx =
    switch (Siblings.neighbors(Siblings.trim_secondary(sibs))) {
    | (_, Some(p))
    | (Some(p), _) =>
      switch (Id.Map.find_opt(Piece.id(p), info_map)) {
      | Some(ci) => Language.Statics.Info.ctx_of(ci)
      | None => Language.Ctx.empty
      }
    | (None, None) =>
      switch (z.relatives.ancestors) {
      | [(a, _), ..._] =>
        switch (Id.Map.find_opt(a.id, info_map)) {
        | Some(ci) => Language.Statics.Info.ctx_of(ci)
        | None => Language.Ctx.empty
        }
      | [] => Language.Ctx.empty
      }
    };
  Some(
    Language.Statics.Info.Secondary({
      id: Id.mk(),
      cls: Secondary(cls),
      sort,
      ctx,
    }),
  );
};

let ci_of =
    (
      ~ws_to_term: Id.Map.t(Id.t)=Id.Map.empty,
      z: ZipperBase.t,
      info_map: Language.Statics.Map.t,
    )
    : option(Language.Statics.Info.t) => {
  /* Try to find term info via whitespace association. When the cursor
   * is on whitespace, look up which term owns it via ws_to_term,
   * then find that term's info in info_map. */
  let ws_lookup = () => {
    let sibs = ZipperBase.sibs_with_sel(z);
    switch (Siblings.neighbors(sibs)) {
    | (_, Some(Secondary(s))) =>
      switch (Id.Map.find_opt(s.id, ws_to_term)) {
      | Some(term_id) => Id.Map.find_opt(term_id, info_map)
      | None => None
      }
    | (Some(Secondary(s)), _) =>
      switch (Id.Map.find_opt(s.id, ws_to_term)) {
      | Some(term_id) => Id.Map.find_opt(term_id, info_map)
      | None => None
      }
    | _ => None
    };
  };

  /* First check if we're at a virtual hole (shape conflict). If so,
   * look up the EmptyHole entry in the info_map by parent tile ID. */
  if (at_virtual_hole(z)) {
    switch (try_find_virtual_hole_ci(z, info_map)) {
    | Some(_) as result => result
    | None =>
      /* Try whitespace association before falling back */
      switch (ws_lookup()) {
      | Some(_) as result => result
      | None =>
        switch (piece''(z)) {
        | Some({piece: p, _}) => Id.Map.find_opt(Piece.id(p), info_map)
        | None => secondary_fallback(z, info_map)
        }
      }
    };
  } else {
    switch (piece''(z)) {
    | Some({piece: p, _}) => Id.Map.find_opt(Piece.id(p), info_map)
    | None =>
      /* Try whitespace association before secondary fallback */
      switch (ws_lookup()) {
      | Some(_) as result => result
      | None => secondary_fallback(z, info_map)
      }
    };
  };
};

/* For type-directed completion (TyDi): returns the ci of the
 * left neighbor tile, which is the token being completed.
 * Falls back to ci_of when no suitable left neighbor exists. */
let ci_for_completion =
    (z: ZipperBase.t, info_map: Language.Statics.Map.t)
    : option(Language.Statics.Info.t) =>
  switch (Siblings.neighbor(Left, z.relatives.siblings)) {
  | Some(p) when !Piece.is_secondary(p) && !Piece.is_hole(p) =>
    Id.Map.find_opt(Piece.id(p), info_map)
  | _ => ci_of(z, info_map)
  };
