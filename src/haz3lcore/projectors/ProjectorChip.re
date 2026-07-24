open Language;

/* The compact stand-in rendered at the code site for a projector whose
 * primary UI has been docked to the sidebar panel (see
 * ProjectorCore.Placement). A chip is a kind glyph followed by a few
 * characters of the projector's underlying syntax.
 *
 * Both the space reserved in the base editor (via
 * ProjectorInfo.ShapeMapSemantics) and the syntax rendered by
 * ProjectorView come from `segment` below, so the placeholder shape
 * can't drift from what is actually drawn. */

/* Characters of underlying syntax shown in the chip */
let available = 10;

/* Columns reserved for the kind glyph and the gap after it */
let glyph_cols = 2;

let inline_settings =
  ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off);

/* Abbreviated rendering of a projector's underlying syntax */
let segment = (p: Base.projector): Base.segment => {
  let seg = Piece.unparenthesize(p.syntax);
  switch (MakeTerm.for_projection(seg)) {
  | Some(Exp(e)) =>
    Abbreviate.abbreviate_exp(~available, e)
    |> fst
    |> ExpToSegment.exp_to_segment(~settings=inline_settings)
  | Some(Pat(pat)) =>
    Abbreviate.abbreviate_pat(~available, pat)
    |> fst
    |> (x => Grammar.Pat(x))
    |> ExpToSegment.any_to_segment(~settings=inline_settings)
  | _ => seg
  };
};

let text = (seg: Base.segment): string =>
  Printer.of_segment(~holes="?", ~indent="", ~is_single_line=true, seg);

/* Space the base editor leaves for a docked projector. Chips are always
 * single-line: the abbreviated segment is built with inline settings. */
let shape = (p: Base.projector): ProjectorCore.Shape.t => {
  let (_, cols) = Util.Unicode.Width.bounding_box_for(text(segment(p)));
  ProjectorCore.Shape.inline(glyph_cols + cols);
};
