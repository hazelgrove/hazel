let tuple_zip: (UHExp.skel, HTyp.t) => option(list((UHExp.skel, HTyp.t)));

/**
 * Get type mode of nth operand of an opseq in synthetic position
 */
let syn_nth_type_mode:
  (Contexts.t, int, UHExp.opseq) => option(Statics.type_mode);
/**
 * Get type mode of nth operand of an opseq in analytic position
 */
let ana_nth_type_mode:
  (Contexts.t, int, UHExp.opseq, HTyp.t) => option(Statics.type_mode);

/**
 * Under context `ctx`, `syn(ctx, e)` synthesizes a type for `e`
 * (if possible)
 */
let syn: (Contexts.t, UHExp.t) => option(HTyp.t);
let syn_block: (Contexts.t, UHExp.block) => option(HTyp.t);
let syn_lines: (Contexts.t, list(UHExp.line)) => option(Contexts.t);
let syn_opseq: (Contexts.t, UHExp.opseq) => option(HTyp.t);
let syn_skel: (Contexts.t, UHExp.skel, UHExp.seq) => option(HTyp.t);
let syn_operand: (Contexts.t, UHExp.operand) => option(HTyp.t);
let syn_rules: (Contexts.t, UHExp.rules, HTyp.t) => option(HTyp.t);
let syn_rule: (Contexts.t, UHExp.rule, HTyp.t) => option(HTyp.t);

/**
 * Under context `ctx`, `ana(ctx, p, ty)` analyzes `e` against `ty`
 */
let ana: (Contexts.t, UHExp.t, HTyp.t) => option(unit);
let ana_opseq: (Contexts.t, UHExp.opseq, HTyp.t) => option(unit);
let ana_skel: (Contexts.t, UHExp.skel, UHExp.seq, HTyp.t) => option(unit);
let ana_splice_map: (Contexts.t, UHExp.splice_map) => option(Contexts.t);

/**
 * Given a pattern `e` in synthetic position under context `ctx`,
 * `syn_fix_holes(ctx, id_gen, e)` fixes the err statuses in `e` such
 * that it can synthesize a type and returns the results of doing so
 */
let syn_fix_holes:
  (Contexts.t, IDGen.t, ~renumber_empty_holes: bool=?, UHExp.t) =>
  (UHExp.t, HTyp.t, IDGen.t);
let syn_fix_holes_block:
  (Contexts.t, IDGen.t, ~renumber_empty_holes: bool=?, UHExp.block) =>
  (UHExp.block, HTyp.t, IDGen.t);
let syn_fix_holes_lines:
  (Contexts.t, IDGen.t, ~renumber_empty_holes: bool=?, list(UHExp.line)) =>
  (list(UHExp.line), Contexts.t, IDGen.t);
let syn_fix_holes_opseq:
  (Contexts.t, IDGen.t, ~renumber_empty_holes: bool=?, UHExp.opseq) =>
  (UHExp.opseq, HTyp.t, IDGen.t);
let syn_fix_holes_operand:
  (Contexts.t, IDGen.t, ~renumber_empty_holes: bool=?, UHExp.operand) =>
  (UHExp.operand, HTyp.t, IDGen.t);
let syn_fix_holes_rules:
  (Contexts.t, IDGen.t, ~renumber_empty_holes: bool=?, UHExp.rules, HTyp.t) =>
  (UHExp.rules, IDGen.t, list(HTyp.t), option(HTyp.t));

/**
 * Given a pattern `e` in analytic position under context `ctx`,
 * `ana_fix_holes(ctx, id_gen, e, ty)` fixes the err statuses in `e`
 * such that it can analyze against `ty` and returns the result of
 * doing so
 */
let ana_fix_holes:
  (Contexts.t, IDGen.t, ~renumber_empty_holes: bool=?, UHExp.t, HTyp.t) =>
  (UHExp.t, IDGen.t);
let ana_fix_holes_block:
  (Contexts.t, IDGen.t, ~renumber_empty_holes: bool=?, UHExp.block, HTyp.t) =>
  (UHExp.block, IDGen.t);
let ana_fix_holes_opseq:
  (Contexts.t, IDGen.t, ~renumber_empty_holes: bool=?, UHExp.opseq, HTyp.t) =>
  (UHExp.opseq, IDGen.t);
let ana_fix_holes_rules:
  (
    Contexts.t,
    IDGen.t,
    ~renumber_empty_holes: bool=?,
    UHExp.rules,
    HTyp.t,
    HTyp.t
  ) =>
  (UHExp.rules, IDGen.t);

let syn_fix_holes_z:
  (Contexts.t, IDGen.t, ZExp.t) => (ZExp.t, HTyp.t, IDGen.t);
let syn_fix_holes_zlines:
  (Contexts.t, IDGen.t, ZExp.zblock) => (ZExp.zblock, Contexts.t, IDGen.t);
let syn_fix_holes_zopseq:
  (Contexts.t, IDGen.t, ZExp.zopseq) => (ZExp.zopseq, HTyp.t, IDGen.t);
let syn_fix_holes_zrules:
  (Contexts.t, IDGen.t, ZExp.zrules, HTyp.t) =>
  (ZExp.zrules, list(HTyp.t), option(HTyp.t), IDGen.t);

let ana_fix_holes_z:
  (Contexts.t, IDGen.t, ZExp.t, HTyp.t) => (ZExp.t, IDGen.t);
let ana_fix_holes_zopseq:
  (Contexts.t, IDGen.t, ZExp.zopseq, HTyp.t) => (ZExp.zopseq, IDGen.t);

let fix_and_renumber_holes:
  (Contexts.t, UHExp.t) => (UHExp.t, HTyp.t, IDGen.t);

let fix_and_renumber_holes_z: (Contexts.t, ZExp.t) => Statics.edit_state;

let joined_pattern_type: (Contexts.t, list(UHExp.rule)) => option(HTyp.t);

/**
 * Currently we restrict recursive definitions to let lines with
 * type annotations that bind a function to a single variable.
 * Given a let line with a pattern `p`, and a defining expression `e`,
 * `extend_let_def_ctx(ctx, p, e)` returns the context available to `e`.
 */
let extend_let_def_ctx: (Contexts.t, UHPat.t, UHExp.t) => Contexts.t;

/**
 * Currently we restrict recursive definitions to let lines with
 * type annotations that bind a function to a single variable.
 * Given a let line with a pattern `p`, and a defining expression `e`,
 * `recursive_let_id(ctx, p, e)` returns the name of the recursive reference, if any.
 */
let recursive_let_id: (Contexts.t, UHPat.t, UHExp.t) => option(Var.t);

/**
 * Extends the provided context, joining the type of the pattern
 * with the type of the defining expression.
 * Precondition: provided pattern and expression have consistent types
 */
let extend_let_body_ctx: (Contexts.t, UHPat.t, UHExp.t) => Contexts.t;
