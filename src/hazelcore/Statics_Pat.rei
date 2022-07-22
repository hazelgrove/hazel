let tuple_zip:
  (UHPat.skel, HTyp.head_normalized) => option(list((UHPat.skel, HTyp.t)));

/**
 * Get type mode of nth operand of an opseq in synthetic position
 */
let syn_nth_type_mode:
  (Context.t, int, UHPat.opseq) => option(Statics.type_mode);
/**
 * Get type mode of nth operand of an opseq in analytic position
 */
let ana_nth_type_mode:
  (Context.t, int, UHPat.opseq, HTyp.t) => option(Statics.type_mode);

/**
 * Under context `ctx`, `syn(ctx, p)` synthesizes a type for `p` and
 * produces a new context with bindings introduced by `p` (if possible)
 */
let syn: (Context.t, UHPat.t) => option((HTyp.t, Context.t, Constraints.t));
let syn_opseq:
  (Context.t, UHPat.opseq) => option((HTyp.t, Context.t, Constraints.t));
let syn_skel:
  (Context.t, UHPat.skel, UHPat.seq) =>
  option((HTyp.t, Context.t, Constraints.t));
let syn_operand:
  (Context.t, UHPat.operand) => option((HTyp.t, Context.t, Constraints.t));

/**
 * Under context `ctx`, `ana(ctx, p, ty)` analyzes `p` against `ty` and
 * produces a new context with bindings introduced by `p` if successful
 */
let ana: (Context.t, UHPat.t, HTyp.t) => option((Context.t, Constraints.t));
let ana_operand:
  (Context.t, UHPat.operand, HTyp.t) => option((Context.t, Constraints.t));
let ana_skel:
  (Context.t, UHPat.skel, UHPat.seq, HTyp.t) =>
  option((Context.t, Constraints.t));

/**
 * Given a pattern `p` in synthetic position under context `ctx`,
 * `syn_fix_holes(ctx, id_gen, p)` fixes the err statuses in `p` such
 * that it can synthesize a type and returns the results of doing so
 */
let syn_fix_holes:
  (Context.t, IDGen.t, ~renumber_empty_holes: bool=?, UHPat.t) =>
  (UHPat.t, HTyp.t, Context.t, IDGen.t, Constraints.t);

/**
 * Given a pattern `p` in analytic position under context `ctx`,
 * `ana_fix_holes(ctx, id_gen, p, ty)` fixes the err statuses in `p`
 * such that it can analyze against `ty` and returns the result of
 * doing so
 */
let ana_fix_holes:
  (Context.t, IDGen.t, ~renumber_empty_holes: bool=?, UHPat.t, HTyp.t) =>
  (UHPat.t, Context.t, IDGen.t, Constraints.t);
let ana_fix_holes_opseq:
  (Context.t, IDGen.t, ~renumber_empty_holes: bool=?, UHPat.opseq, HTyp.t) =>
  (UHPat.opseq, Context.t, IDGen.t, Constraints.t);
let ana_fix_holes_operand:
  (Context.t, IDGen.t, ~renumber_empty_holes: bool=?, UHPat.operand, HTyp.t) =>
  (UHPat.operand, Context.t, IDGen.t, Constraints.t);
let syn_fix_holes_z:
  (Context.t, IDGen.t, ZPat.t) =>
  (ZPat.t, HTyp.t, Context.t, IDGen.t, Constraints.t);

let ana_fix_holes_z:
  (Context.t, IDGen.t, ZPat.t, HTyp.t) =>
  (ZPat.t, Context.t, IDGen.t, Constraints.t);
