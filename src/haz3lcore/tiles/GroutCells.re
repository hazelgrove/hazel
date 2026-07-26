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
 *   LineEndPadded  as LineEndFree, but the anchor token wants a
 *               separating space (PadStyle) and none is there: the
 *               hole draws one cell FURTHER right, leaving a blank
 *               cell between token and glyph (`then ?`, vs `f(?`
 *               after an opener). Two columns, both past the line's
 *               text, so nothing is displaced. The pad is a
 *               RENDERING property, never a piece: minting a space
 *               here would break placement's purity (strip removes
 *               only grout, so a minted pad survives its own strip)
 *               and its determinism (a space needs a random id).
 *   Pinch       grout squeezed directly between content: zero-width
 *               boundary mark
 *
 * Width transfer (consumed by Measured): NextSpace/PrevSpace/
 * LineEndFree grout measures 1 column and its consumed space (if
 * any) measures 0, so total line width equals the grout-free
 * segment's — measured-level layout invisibility — except LineEndFree
 * adds one column past the line's content (LineEndPadded two).
 * Pinch measures 0. */

[@deriving (show({with_path: false}), sexp)]
type cls =
  | NextSpace
  | PrevSpace
  | LineEndFree
  | LineEndPadded
  | Pinch;

type t = {
  classes: Id.Map.t(cls),
  consumed: Id.Map.t(unit),
  /* space id -> the hole that consumed its cell */
  consumed_by: Id.Map.t(Id.t),
};

let empty: t = {
  classes: Id.Map.empty,
  consumed: Id.Map.empty,
  consumed_by: Id.Map.empty,
};

let cls_of = (cells: t, id: Id.t): option(cls) =>
  Id.Map.find_opt(id, cells.classes);

let is_consumed = (cells: t, id: Id.t): bool =>
  Id.Map.mem(id, cells.consumed);

let consumer_of = (cells: t, id: Id.t): option(Id.t) =>
  Id.Map.find_opt(id, cells.consumed_by);

/* flat adjacency stream; ids kept for grout and spaces (the pieces
 * classification assigns to); everything else is Content or Break */
type atom =
  | Content(string) /* a token's text: the pad rule's left operand */
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
      | Secondary(w) => [Content(Secondary.get_string(w.content))]
      | Tile(t) =>
        Aba.mk(t.shards, t.children)
        |> Aba.join(
             i =>
               [
                 Content(
                   switch (List.nth_opt(t.label, i)) {
                   | Some(tok) => tok
                   | None => ""
                   },
                 ),
               ],
             atoms,
           )
        |> List.concat
      | Projector(_) => [Content("")]
      },
    seg,
  );

/* the original weave fold, recording decisions instead of strings.
 * A grout's PrevSpace claim on the preceding space only holds when
 * the grout is followed by content (else the following space or the
 * line end hosts it, and the preceding space stays real). */
let classify = (seg: segment): t => {
  /* a trailing hole pads iff its ANCHOR TOKEN wants separation and no
     space already provides it. Preceded by a space (the user's, or
     one the display oracle already minted for span material) the
     space separates and the hole takes the plain free cell — which is
     what keeps this from double-padding. Preceded by a linebreak or
     nothing there is no anchor to separate from. */
  let line_end_cls = (prev: option(atom)): cls =>
    switch (prev) {
    | Some(Content(lt)) when PadStyle.pad(lt, PadStyle.hole_token) =>
      LineEndPadded
    | _ => LineEndFree
    };
  let rec go = (~prev: option(atom)=None, cells: t, ats: list(atom)): t =>
    switch (ats) {
    | [S(sid), G(gid, _) as g, ...tl]
        when
          switch (tl) {
          | []
          | [S(_), ..._]
          | [Break, ..._] => false
          | _ => true
          } =>
      go(
        ~prev=Some(g),
        {
          classes: Id.Map.add(gid, PrevSpace, cells.classes),
          consumed: Id.Map.add(sid, (), cells.consumed),
          consumed_by: Id.Map.add(sid, gid, cells.consumed_by),
        },
        tl,
      )
    | [G(gid, _), S(sid) as sp, ...tl] =>
      go(
        ~prev=Some(sp),
        {
          classes: Id.Map.add(gid, NextSpace, cells.classes),
          consumed: Id.Map.add(sid, (), cells.consumed),
          consumed_by: Id.Map.add(sid, gid, cells.consumed_by),
        },
        tl,
      )
    | [G(gid, _)] => {
        ...cells,
        classes: Id.Map.add(gid, line_end_cls(prev), cells.classes),
      }
    | [G(gid, _) as g, ...[Break, ..._] as tl] =>
      go(
        ~prev=Some(g),
        {
          ...cells,
          classes: Id.Map.add(gid, line_end_cls(prev), cells.classes),
        },
        tl,
      )
    | [G(gid, _) as g, ...tl] =>
      go(
        ~prev=Some(g),
        {
          ...cells,
          classes: Id.Map.add(gid, Pinch, cells.classes),
        },
        tl,
      )
    | [a, ...tl] => go(~prev=Some(a), cells, tl)
    | [] => cells
    };
  go(empty, atoms(seg));
};

/* the measured-faithful piece stream: consumed spaces removed and
 * trailing pads materialised, so a classification-blind printer
 * (Printer.of_segment) yields text whose columns match Measured
 * except one leading char per Pinch hole — callers that place markers
 * at measured columns shift by the Pinch count on the row and nothing
 * else.
 *
 * PRINT-ONLY. The pad space injected here exists for the duration of
 * one render; it is never placed, stored, analysed or compared, so it
 * cannot affect placement's purity (both callers hand the result
 * straight to a printer). The pad is a rendering property — this is
 * where rendering happens. */
let drop_consumed_spaces = (seg: segment): segment => {
  let cells = classify(seg);
  let pad = (): piece => Secondary(Secondary.mk_space(Id.mk()));
  let rec go = (sg: segment): segment =>
    List.concat_map(
      (p: piece) =>
        switch (p) {
        | Secondary(w) when Secondary.is_space(w) && is_consumed(cells, w.id) =>
          []
        | Grout(g) when cls_of(cells, g.id) == Some(LineEndPadded) => [
            pad(),
            p,
          ]
        | Tile(t) => [
            Tile({
              ...t,
              children: List.map(go, t.children),
            }),
          ]
        | p => [p]
        },
      sg,
    );
  go(seg);
};

/* Pinch-class grout on `row` strictly left of (or at, when ~incl)
 * `col` — the marker shift for classification-blind printed text */
let pinch_shift =
    (cells: t, ~grout_positions: list((Id.t, int, int)), ~incl, ~row, ~col)
    : int =>
  grout_positions
  |> List.filter(((id, r, c)) =>
       cls_of(cells, id) == Some(Pinch)
       && r == row
       && (incl ? c <= col : c < col)
     )
  |> List.length;

/* does this class occupy a cell (width 1) or none (width 0)? */
let width = (c: cls): int =>
  switch (c) {
  | NextSpace
  | PrevSpace
  | LineEndFree => 1
  | LineEndPadded => 2 /* blank pad cell + the glyph's cell */
  | Pinch => 0
  };

/* does this class occupy the free space past a line's text? */
let is_line_end = (c: cls): bool =>
  switch (c) {
  | LineEndFree
  | LineEndPadded => true
  | NextSpace
  | PrevSpace
  | Pinch => false
  };
