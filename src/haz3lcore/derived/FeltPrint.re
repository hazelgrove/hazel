open Util;
open Base;

/* FELT-PRINT: text that approximates how a segment will look ON
 * SCREEN under the zero-width-grout model — the assessment tool the
 * design refinement calls for ("looking and feeling good" is the
 * watchword; this is how it gets checked in text).
 *
 * The model: grout pieces are conceptually ZERO-WIDTH; the view gives
 * a hole width only where there is room. Rendering a PLACED segment
 * (GroutPlace output — this renderer derives nothing, it just shows
 * the pieces):
 *
 *   1. grout followed by a space  -> sigil drawn IN that space's cell
 *      (the cell the placement policy targeted; no width added)
 *   2. grout at a line end (before a linebreak or the segment end)
 *      -> sigil in the free cell past the text (width added only
 *      where nothing follows — perceptual, per the spec)
 *   3. grout after a space, before a token -> sigil drawn in the
 *      PRECEDING space's cell (a single-space gap holds the hole)
 *   4. grout pinched between tokens -> zero-width thin sigil,
 *      inserted (the one place columns diverge from the screen)
 *
 * LAYOUT INVISIBILITY (the shut property this renders checkable):
 * with consuming sigils mapped back to a space and non-consuming
 * sigils mapped to nothing, the felt render of a placed segment is
 * EXACTLY the plain render of its grout-stripped segment — grout
 * contributes nothing to layout, so its churn cannot displace text
 * or caret. Sigils: ? ~ thick (occupy a cell), ‽ ∻ thin. */

[@deriving (show({with_path: false}), sexp)]
type cell =
  | Consumed /* rules 1/3: sigil occupies an existing space cell */
  | Free /* rule 2: free cell at a line end */
  | Pinched; /* rule 4: zero-width between tokens */

let sigil_default = (cell: cell, shape: Grout.shape): string =>
  switch (cell, shape) {
  | (Consumed | Free, Convex) => "?"
  | (Consumed | Free, Concave) => "~"
  | (Pinched, Convex) => {|‽|}
  | (Pinched, Concave) => {|∻|}
  };

/* the weave rules moved to GroutCells.classify — THE one home for
 * cell assignment, shared with Measured and the view. This renderer
 * just prints pieces under that classification: consumed spaces
 * print as nothing, each grout prints its sigil in the cell class
 * the classification assigned. */
let cell_of = (c: GroutCells.cls): cell =>
  switch (c) {
  | NextSpace
  | PrevSpace => Consumed
  | LineEndFree => Free
  | Pinch => Pinched
  };

/* No indent pass: on this branch indentation is STORED as space
 * pieces in the segment (auto-indent/Format materialize it), so
 * rendering pieces as-is IS the screen layout. */
let render =
    (
      ~sigil=sigil_default,
      ~projector_to_segment=Triggers.projector_to_invoke,
      seg: Segment.t,
    )
    : string => {
  let cells = GroutCells.classify(seg);
  let rec go = (sg: segment): string =>
    sg
    |> List.map((p: piece) =>
         switch (p) {
         | Grout(g) =>
           let c =
             GroutCells.cls_of(cells, g.id)
             |> Option.value(~default=GroutCells.Pinch);
           sigil(cell_of(c), g.shape);
         | Secondary(w) when Secondary.is_space(w) =>
           GroutCells.is_consumed(cells, w.id) ? "" : " "
         | Secondary(w) when Secondary.is_linebreak(w) => "\n"
         | Secondary(w) => Secondary.get_string(w.content)
         | Tile(t) =>
           Aba.mk(t.shards, t.children)
           |> Aba.join(i => List.nth(t.label, i), go)
           |> String.concat("")
         | Projector(pr) => go(projector_to_segment(pr))
         }
       )
    |> String.concat("");
  go(seg);
};

/* MEASURED-FAITHFUL PRINT — the one home for every harness/probe/
 * fallback-check text render whose columns must line up with
 * Measured under width transfer. Consumed spaces are omitted (their
 * cell belongs to the hole, which prints its ?/~ there), so string
 * indices equal measured columns EXCEPT one printed char per
 * zero-width Pinch hole; measured_caret maps a measured point to
 * its printed column (strictly-before pinch count). Callers insert
 * markers at measured_caret points and never shift themselves. */
let measured_print =
    (~holes="?", ~concave_holes="~", ~measured, seg: Segment.t): string =>
  Printer.of_segment(
    ~holes,
    ~concave_holes,
    ~indent="",
    ~measured,
    GroutCells.drop_consumed_spaces(seg),
  );

let measured_caret =
    (~measured: Measured.t, seg: Segment.t, pt: Util.Point.t): Util.Point.t => {
  let cells = GroutCells.classify(seg);
  let grout_positions = {
    let rec go = (sg: segment) =>
      List.concat_map(
        (p: piece) =>
          switch (p) {
          | Grout(g) =>
            switch (Measured.find_g(g, measured)) {
            | m => [(g.id, m.origin.row, m.origin.col)]
            | exception _ => []
            }
          | Tile(t) => List.concat_map(go, t.children)
          | _ => []
          },
        sg,
      );
    go(seg);
  };
  {
    ...pt,
    col:
      pt.col
      + GroutCells.pinch_shift(
          cells,
          ~grout_positions,
          ~incl=false,
          ~row=pt.row,
          ~col=pt.col,
        ),
  };
};

/* the layout-invisibility left side: consuming sigils restore their
 * cell, non-consuming sigils vanish — must equal render(strip(seg)) */
let render_ghostless = (seg: Segment.t): string =>
  render(
    ~sigil=
      (cell, _) =>
        switch (cell) {
        | Consumed => " "
        | Free
        | Pinched => ""
        },
    seg,
  );

/* The editor view of an edit state, EDITOR-FAITHFUL: the placed
 * display segment is measured under width transfer, the caret point
 * is computed exactly as the editor computes it (Zipper.Caret.point
 * against the DISPLAY measured), and the marker lands via
 * measured_caret. A caret bug in the live pipeline reproduces here
 * by construction. */
let of_zipper = (~caret="¦", z: Zipper.t): string => {
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let placed = GroutPlace.place(seg);
  let measured = Measured.of_segment(placed, Id.Map.empty, Id.Map.empty);
  let point: Point.t = Zipper.Caret.point(measured, z);
  let felt_point = measured_caret(~measured, placed, point);
  measured_print(~measured, placed)
  |> String.split_on_char('\n')
  |> Printer.insert_string(caret, felt_point)
  |> String.concat("\n");
};
