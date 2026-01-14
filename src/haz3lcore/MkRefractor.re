open Util;

/* Create refractor entries and projectors.
 *
 * Refractors store a simplified `entry` type in Zipper.Refractor.Map
 * (just kind + model), avoiding redundant id/syntax in serialization.
 * When the full Base.projector is needed for rendering, use `to_projector`.
 *
 * NOTE: This module exists separately from ProbePerform due to a
 * dependency cycle: Triggers -> ProbePerform would create a cycle
 * through Printer -> ProjectorInfo -> CachedSyntax -> ProbePerform. */

/* Create a simplified entry for storage in refractor maps */
let mk_entry = (kind: ProjectorCore.Kind.t): Zipper.Refractor.entry => {
  let (module P) = ProjectorInit.to_module(kind);
  /* Create dummy syntax just to get the initial model string */
  let grout = Piece.mk_grout(~id=Id.invalid, Convex);
  let seg = [grout];
  let any =
    MakeTerm.for_projection(seg)
    |> OptUtil.get_or_fail("MkRefractor.mk_entry: maketerm");
  let model =
    P.init(any) |> OptUtil.get_or_fail("MkRefractor.mk_entry: init");
  {
    kind,
    model,
  };
};

/* Construct full Base.projector from entry and id, for rendering.
 * Creates dummy syntax with Id.invalid since refractors use skip_inline=true
 * and don't actually display the syntax. */
let to_projector = (id: Id.t, entry: Zipper.Refractor.entry): Base.projector => {
  /* Create minimal dummy syntax with consistent invalid IDs.
   * The parenthesization is kept for compatibility with the projector API,
   * but since refractors use skip_inline=true, this is never displayed. */
  let grout = Piece.mk_grout(~id=Id.invalid, Convex);
  let seg = [grout];
  /* Inline parenthesization logic using Id.invalid.
   * Segment.parenthesize calls Piece.mk_tile which always generates
   * a fresh ID, so we construct the Tile record directly. */
  let sort = Segment.sort_of(Segment.skel(seg), seg);
  let form = Form.mk_parens(sort);
  let piece: Base.piece =
    Tile({
      id: Id.invalid,
      label: form.label,
      mold: form.mold,
      shards: List.mapi((i, _) => i, form.label),
      children: [seg],
    });
  ProjectorCore.mk(~id, entry.kind, piece, entry.model);
};

/* TODO: Consider simplifying dummy syntax further - since skip_inline=true
 * for refractors, we may not need parenthesization at all. The minimal
 * piece would be Secondary (empty whitespace) rather than grout+parens. */

let add_single = (id: Id.t, z: Zipper.t): Zipper.t =>
  Zipper.update_manuals(Id.Map.add(id, mk_entry(Probe)), z);
