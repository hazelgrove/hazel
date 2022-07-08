let tuple_zip: (UHPat.skel, HTyp.t) => option(list((UHPat.skel, HTyp.t)));

/**
 * Get type mode of nth operand of an opseq in synthetic position
 */
let syn_nth_type_mode:
  (Contexts.t, int, UHPat.opseq) => option(Statics.type_mode);
/**
 * Get type mode of nth operand of an opseq in analytic position
 */
let ana_nth_type_mode:
  (Contexts.t, int, UHPat.opseq, HTyp.t) => option(Statics.type_mode);

/**
 * Under context `ctx`, `syn(ctx, p)` synthesizes a type for `p` and
 * produces a new context with bindings introduced by `p` (if possible)
 */
let syn: (Contexts.t, UHPat.t) => option((HTyp.t, Contexts.t));
let syn_opseq: (Contexts.t, UHPat.opseq) => option((HTyp.t, Contexts.t));
let syn_skel:
  (Contexts.t, UHPat.skel, UHPat.seq) => option((HTyp.t, Contexts.t));
let syn_operand: (Contexts.t, UHPat.operand) => option((HTyp.t, Contexts.t));

/**
 * Under context `ctx`, `ana(ctx, p, ty)` analyzes `p` against `ty` and
 * produces a new context with bindings introduced by `p` if successful
 */
let ana: (Contexts.t, UHPat.t, HTyp.t) => option(Contexts.t);
let ana_operand: (Contexts.t, UHPat.operand, HTyp.t) => option(Contexts.t);
let ana_skel:
  (Contexts.t, UHPat.skel, UHPat.seq, HTyp.t) => option(Contexts.t);

/**
 * Given a pattern `p` in synthetic position under context `ctx`,
 * `syn_fix_holes(ctx, id_gen, p)` fixes the err statuses in `p` such
 * that it can synthesize a type and returns the results of doing so
 */
let syn_fix_holes:
  (Contexts.t, IDGen.t, ~renumber_empty_holes: bool=?, UHPat.t) =>
  (UHPat.t, HTyp.t, Contexts.t, IDGen.t);

/**
 * Given a pattern `p` in analytic position under context `ctx`,
 * `ana_fix_holes(ctx, id_gen, p, ty)` fixes the err statuses in `p`
 * such that it can analyze against `ty` and returns the result of
 * doing so
 */
let ana_fix_holes:
  (Contexts.t, IDGen.t, ~renumber_empty_holes: bool=?, UHPat.t, HTyp.t) =>
  (UHPat.t, Contexts.t, IDGen.t);
let ana_fix_holes_opseq:
  (Contexts.t, IDGen.t, ~renumber_empty_holes: bool=?, UHPat.opseq, HTyp.t) =>
  (UHPat.opseq, Contexts.t, IDGen.t);
let ana_fix_holes_operand:
  (
    Contexts.t,
    IDGen.t,
    ~renumber_empty_holes: bool=?,
    UHPat.operand,
    HTyp.t
  ) =>
  (UHPat.operand, Contexts.t, IDGen.t);
let syn_fix_holes_z:
  (Contexts.t, IDGen.t, ZPat.t) => (ZPat.t, HTyp.t, Contexts.t, IDGen.t);

let ana_fix_holes_z:
  (Contexts.t, IDGen.t, ZPat.t, HTyp.t) => (ZPat.t, Contexts.t, IDGen.t);

let case_syn:
  (Contexts.t, UHPat.t) => option((HTyp.t, Contexts.t, Constraints.t));

let generate_rules_constraints:
  (Contexts.t, list(UHPat.t), HTyp.t) => list(Constraints.t);

/** for compile */
let case_ana_operand:
  (Contexts.t, UHPat.operand, HTyp.t) => option((Contexts.t, Constraints.t));

/** for compile */
let case_ana_skel:
  (Contexts.t, UHPat.skel, UHPat.seq, HTyp.t) =>
  option((Contexts.t, Constraints.t));

let generate_one_constraints:
  (Contexts.t, list(UHPat.t), HTyp.t) => Constraints.t;
