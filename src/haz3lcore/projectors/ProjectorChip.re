open Language;

/* The stand-in rendered at the code site for a projector docked to the
 * sidebar (see ProjectorCore.Placement): one fixed glyph, same for every
 * kind. `shape` is what the editor reserves and `glyph_cols` is what the
 * chip is drawn at (proj-placement.css), so the two can't drift. */

let glyph = {|⇥|};

let glyph_cols = 2;

/* Syntax budget for the sidebar card header — not the chip, which is always
   glyph_cols wide. The panel body is ~38 columns; the rest is padding, gap
   and the undock button. */
let card_available = 32;

let inline_settings =
  ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off);

/* Abbreviated syntax for the sidebar card header */
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

let shape = (_: Base.projector): ProjectorCore.Shape.t =>
  ProjectorCore.Shape.inline(glyph_cols);
