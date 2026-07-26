open Language;

/* The compact stand-in rendered at the code site for a projector whose
 * primary UI has been docked to the sidebar panel (see
 * ProjectorCore.Placement). A chip is one fixed glyph, the same for
 * every projector kind, like the fold projector's ⋱.
 *
 * `shape` is what the base editor reserves (via
 * ProjectorInfo.ShapeMapSemantics) and `glyph_cols` is what the chip is
 * drawn at (proj-placement.css), so the two can't drift.
 *
 * `segment` is the abbreviated syntax shown in the sidebar card header;
 * it is not part of the chip. */

/* The chip glyph: an arrow into a bar, for "docked off to the right" */
let glyph = {|⇥|};

/* Columns the chip occupies, glyph included */
let glyph_cols = 2;

/* Characters of underlying syntax shown in the sidebar card header. Budget
 * only: the chip's own width is glyph_cols, independent of this. The panel
 * body is ~38 columns wide; this leaves room for padding, gap and the
 * undock button. */
let card_available = 32;

let inline_settings =
  ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off);

/* Abbreviated rendering of a projector's underlying syntax, for the
 * sidebar card header */
let segment = (p: Base.projector): Base.segment => {
  let seg = Piece.unparenthesize(p.syntax);
  switch (MakeTerm.for_projection(seg)) {
  | Some(Exp(e)) =>
    Abbreviate.abbreviate_exp(~available=card_available, e)
    |> fst
    |> ExpToSegment.exp_to_segment(~settings=inline_settings)
  | Some(Pat(pat)) =>
    Abbreviate.abbreviate_pat(~available=card_available, pat)
    |> fst
    |> (x => Grammar.Pat(x))
    |> ExpToSegment.any_to_segment(~settings=inline_settings)
  | _ => seg
  };
};

/* Space the base editor leaves for a docked projector: one glyph, on one
 * line, regardless of kind or syntax. */
let shape = (_: Base.projector): ProjectorCore.Shape.t =>
  ProjectorCore.Shape.inline(glyph_cols);
