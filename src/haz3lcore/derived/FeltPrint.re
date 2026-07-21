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

/* flat render stream: tiles and comments are Text, whitespace keeps
 * its identity so the sigil rules can see cells */
type atom =
  | Text(string)
  | Space
  | Linebreak
  | G(Grout.shape);

let rec atoms =
        (~projector_to_segment: projector => segment, seg: segment)
        : list(atom) =>
  List.concat_map(
    (p: piece) =>
      switch (p) {
      | Grout(g) => [G(g.shape)]
      | Secondary(w) when Secondary.is_space(w) => [Space]
      | Secondary(w) when Secondary.is_linebreak(w) => [Linebreak]
      | Secondary(w) => [Text(Secondary.get_string(w.content))]
      | Tile(t) =>
        Aba.mk(t.shards, t.children)
        |> Aba.join(
             i => [Text(List.nth(t.label, i))],
             kid => atoms(~projector_to_segment, kid),
           )
        |> List.concat
      | Projector(pr) =>
        atoms(~projector_to_segment, projector_to_segment(pr))
      },
    seg,
  );

let rec weave = (~sigil, ats: list(atom)): string =>
  switch (ats) {
  /* rule 3 guarded so rules 1/2 win when the grout's own following
   * cell can host it */
  | [Space, G(sh), ...tl]
      when
        switch (tl) {
        | []
        | [Space, ..._]
        | [Linebreak, ..._] => false
        | _ => true
        } =>
    sigil(Consumed, sh) ++ weave(~sigil, tl)
  | [G(sh), Space, ...tl] => sigil(Consumed, sh) ++ weave(~sigil, tl)
  | [G(sh)] => sigil(Free, sh)
  | [G(sh), ...[Linebreak, ..._] as tl] =>
    sigil(Free, sh) ++ weave(~sigil, tl)
  | [G(sh), ...tl] => sigil(Pinched, sh) ++ weave(~sigil, tl)
  | [Text(s), ...tl] => s ++ weave(~sigil, tl)
  | [Space, ...tl] => " " ++ weave(~sigil, tl)
  | [Linebreak, ...tl] => "\n" ++ weave(~sigil, tl)
  | [] => ""
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
    : string =>
  weave(~sigil, atoms(~projector_to_segment, seg));

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

/* The future-editor view of an edit state: place the (stripped)
 * edit segment fresh, render felt, and mark the caret. The caret's
 * felt column is its raw column minus the width of raw grout before
 * it on its row (grout is width 1 in today's measured, 0 felt). */
let of_zipper = (~caret="¦", z: Zipper.t): string => {
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let raw_measured = Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
  let point: Point.t = Zipper.Caret.point(raw_measured, z);
  let grout_before = {
    let rec go = (sg: segment, acc: int): int =>
      List.fold_left(
        (acc, p: piece) =>
          switch (p) {
          | Grout(g) =>
            switch (Measured.find_g(g, raw_measured)) {
            | m when m.origin.row == point.row && m.origin.col < point.col =>
              acc + 1
            | _ => acc
            | exception _ => acc
            }
          | Tile(t) => List.fold_left((a, k) => go(k, a), acc, t.children)
          | _ => acc
          },
        acc,
        sg,
      );
    go(seg, 0);
  };
  let felt_point = {
    ...point,
    col: point.col - grout_before,
  };
  render(GroutPlace.place(seg))
  |> String.split_on_char('\n')
  |> Printer.insert_string(caret, felt_point)
  |> String.concat("\n");
};
