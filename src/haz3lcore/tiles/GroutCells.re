open Util;
open Base;

/* THE one home for hole-cell assignment (obligation-display design,
 * P5a): given a segment, classify every grout piece into the cell it
 * paints, and record which space pieces are CONSUMED (their cell is
 * the hole's cell, so they render/measure as nothing). Measured,
 * FeltPrint, and the view all consume this classification — no
 * consumer re-derives adjacency on its own.
 *
 * Classes (precedence encoded in the fold, matching FeltPrint's
 * original weave rules byte-for-byte):
 *   NextSpace   grout followed by a space: paints into that space's
 *               cell (the following space is consumed)
 *   PrevSpace   grout preceded by a space and followed by content:
 *               paints into the preceding space's cell
 *   LineEndFree grout before a linebreak or at segment end: paints
 *               into the free cell past the text (the one place a
 *               hole may add perceptual width, per the design doc)
 *   Pinch       grout squeezed directly between content: zero-width
 *               boundary mark
 *
 * Width transfer (consumed by Measured): NextSpace/PrevSpace/
 * LineEndFree grout measures 1 column and its consumed space (if
 * any) measures 0, so total line width equals the grout-free
 * segment's — measured-level layout invisibility — except LineEndFree
 * adds one column past the line's content. Pinch measures 0. */

[@deriving (show({with_path: false}), sexp)]
type cls =
  | NextSpace
  | PrevSpace
  | LineEndFree
  | Pinch;

type t = {
  classes: Id.Map.t(cls),
  consumed: Id.Map.t(unit),
};

let empty: t = {
  classes: Id.Map.empty,
  consumed: Id.Map.empty,
};

let cls_of = (cells: t, id: Id.t): option(cls) =>
  Id.Map.find_opt(id, cells.classes);

let is_consumed = (cells: t, id: Id.t): bool =>
  Id.Map.mem(id, cells.consumed);

/* flat adjacency stream; ids kept for grout and spaces (the pieces
 * classification assigns to); everything else is Content or Break */
type atom =
  | Content
  | Break
  | S(Id.t)
  | G(Id.t, Grout.shape);

let rec atoms = (seg: segment): list(atom) =>
  List.concat_map(
    (p: piece) =>
      switch (p) {
      | Grout(g) => [G(g.id, g.shape)]
      | Secondary(w) when Secondary.is_space(w) => [S(w.id)]
      | Secondary(w) when Secondary.is_linebreak(w) => [Break]
      | Secondary(_) => [Content]
      | Tile(t) =>
        Aba.mk(t.shards, t.children)
        |> Aba.join(_ => [Content], atoms)
        |> List.concat
      | Projector(_) => [Content]
      },
    seg,
  );

/* the original weave fold, recording decisions instead of strings.
 * A grout's PrevSpace claim on the preceding space only holds when
 * the grout is followed by content (else the following space or the
 * line end hosts it, and the preceding space stays real). */
let classify = (seg: segment): t => {
  let rec go = (cells: t, ats: list(atom)): t =>
    switch (ats) {
    | [S(sid), G(gid, _), ...tl]
        when
          switch (tl) {
          | []
          | [S(_), ..._]
          | [Break, ..._] => false
          | _ => true
          } =>
      go(
        {
          classes: Id.Map.add(gid, PrevSpace, cells.classes),
          consumed: Id.Map.add(sid, (), cells.consumed),
        },
        tl,
      )
    | [G(gid, _), S(sid), ...tl] =>
      go(
        {
          classes: Id.Map.add(gid, NextSpace, cells.classes),
          consumed: Id.Map.add(sid, (), cells.consumed),
        },
        tl,
      )
    | [G(gid, _)] => {
        ...cells,
        classes: Id.Map.add(gid, LineEndFree, cells.classes),
      }
    | [G(gid, _), ...[Break, ..._] as tl] =>
      go(
        {
          ...cells,
          classes: Id.Map.add(gid, LineEndFree, cells.classes),
        },
        tl,
      )
    | [G(gid, _), ...tl] =>
      go(
        {
          ...cells,
          classes: Id.Map.add(gid, Pinch, cells.classes),
        },
        tl,
      )
    | [_, ...tl] => go(cells, tl)
    | [] => cells
    };
  go(empty, atoms(seg));
};

/* does this class occupy a cell (width 1) or none (width 0)? */
let width = (c: cls): int =>
  switch (c) {
  | NextSpace
  | PrevSpace
  | LineEndFree => 1
  | Pinch => 0
  };
