open Util;

/* Create a refractor (probe) projector for a given syntax ID.
 * This constructs a Base.projector with dummy syntax using Id.invalid
 * to avoid generating random IDs in serialization.
 *
 * NOTE: This module exists separately from ProbePerform due to a
 * dependency cycle: Triggers -> ProbePerform would create a cycle
 * through Printer -> ProjectorInfo -> CachedSyntax -> ProbePerform. */
let mk = (kind, id): Base.projector => {
  let (module P) = ProjectorInit.to_module(kind);
  /* Create minimal dummy syntax with consistent invalid IDs.
   * The parenthesization is currently kept for compatibility with
   * the projector API, but since refractors use skip_inline=true,
   * this syntax is never actually displayed. */
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
  let any =
    MakeTerm.for_projection(seg)
    |> OptUtil.get_or_fail("MkRefractor.mk: maketerm");
  let model = P.init(any) |> OptUtil.get_or_fail("MkRefractor.mk: init");
  ProjectorCore.mk(~id, kind, piece, model);
};

/* TODO: Consider simplifying dummy syntax further - since skip_inline=true
 * for refractors, we may not need parenthesization at all. The minimal
 * piece would be Secondary (empty whitespace) rather than grout+parens. */

let add_single = (id: Id.t, z: Zipper.t): Zipper.t =>
  Zipper.update_manuals(Id.Map.add(id, mk(Probe, id)), z);
