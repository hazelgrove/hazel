open Util_web;
open OptUtil.Syntax;

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
   Ignores secondary. Used by CaretDec, Arms, CodeEditable, Backpack. */
let for_decoration = indicated(~no_ws=true, ~ign=Piece.is_secondary);

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
      | Grout(_)
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

let ci_of =
    (z: ZipperBase.t, info_map: Language.Statics.Map.t)
    : option(Language.Statics.Info.t) =>
  /* First try the decoration indication function. If it succeeds,
   * look up the piece's info. If not (e.g. only secondary neighbors),
   * create a 'virtual' info map entry for the secondary notation,
   * borrowing semantic context from a nearby 'proxy' term. */
  switch (for_decoration(z)) {
  | Some({piece, _}) => Id.Map.find_opt(Piece.id(piece), info_map)
  | None =>
    let sibs = ZipperBase.sibs_with_sel(z);
    let* cls =
      switch (Siblings.neighbors(sibs)) {
      /* If on side of comment, say we're on comment */
      | (Some(Secondary(sl)), Some(Secondary(_)))
          when Secondary.is_comment(sl) =>
        Some(Language.Secondary.cls_of(sl))
      | (Some(Secondary(_)), Some(Secondary(sr)))
          when Secondary.is_comment(sr) =>
        Some(Language.Secondary.cls_of(sr))
      | (_, Some(Secondary(s)))
      | (Some(Secondary(s)), _) => Some(Language.Secondary.cls_of(s))
      | _ => None
      };
    let* proxy_id =
      switch (Siblings.neighbors(Siblings.trim_secondary(sibs))) {
      | (_, Some(p))
      | (Some(p), _) => Some(Piece.id(p))
      | _ => None
      };
    let+ ci = Id.Map.find_opt(proxy_id, info_map);
    Language.Statics.Info.Secondary({
      id: proxy_id,
      cls: Secondary(cls),
      sort: Language.Statics.Info.sort_of(ci),
      ctx: Language.Statics.Info.ctx_of(ci),
    });
  };

/* For type-directed completion (TyDi): returns the ci of the
 * left neighbor tile, which is the token being completed.
 * Falls back to ci_of when no suitable left neighbor exists. */
let ci_for_completion =
    (z: ZipperBase.t, info_map: Language.Statics.Map.t)
    : option(Language.Statics.Info.t) =>
  switch (Siblings.neighbor(Left, z.relatives.siblings)) {
  | Some(p) when !Piece.is_secondary(p) && !Piece.is_grout(p) =>
    Id.Map.find_opt(Piece.id(p), info_map)
  | _ => ci_of(z, info_map)
  };
