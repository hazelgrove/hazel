let tuple_zip:
  (UHExp.skel, HTyp.head_normalized) => option(list((UHExp.skel, HTyp.t)));

/**
 * Get type mode of nth operand of an opseq in synthetic position
 */
let syn_nth_type_mode:
  (Context.t, int, UHExp.opseq) => option(Statics.type_mode);
/**
 * Get type mode of nth operand of an opseq in analytic position
 */
let ana_nth_type_mode:
  (Context.t, int, UHExp.opseq, HTyp.t) => option(Statics.type_mode);

/**
 * Under context `ctx`, `syn(ctx, e)` synthesizes a type for `e`
 * (if possible)
 */
let syn: (Context.t, UHExp.t) => option(HTyp.t);
let syn_block: (Context.t, UHExp.block) => option(HTyp.t);
let syn_lines: (Context.t, list(UHExp.line)) => option(Context.t);
let syn_opseq: (Context.t, UHExp.opseq) => option(HTyp.t);
let syn_skel: (Context.t, UHExp.skel, UHExp.seq) => option(HTyp.t);
let syn_operand: (Context.t, UHExp.operand) => option(HTyp.t);
let syn_rules:
  (Context.t, UHExp.rules, HTyp.t) => option((HTyp.t, Constraints.t));
let syn_rule:
  (Context.t, UHExp.rule, HTyp.t, Constraints.t) =>
  option((HTyp.t, Constraints.t));

/**
 * Under context `ctx`, `ana(ctx, p, ty)` analyzes `e` against `ty`
 */
let ana: (Context.t, UHExp.t, HTyp.t) => option(unit);
let ana_skel: (Context.t, UHExp.skel, UHExp.seq, HTyp.t) => option(unit);

/**
 * Given a pattern `e` in synthetic position under context `ctx`,
 * `syn_fix_holes(ctx, id_gen, e)` fixes the err statuses in `e` such
 * that it can synthesize a type and returns the results of doing so
 */
let syn_fix_holes:
  (Context.t, IDGen.t, ~renumber_empty_holes: bool=?, UHExp.t) =>
  (UHExp.t, HTyp.t, IDGen.t);
let syn_fix_holes_block:
  (Context.t, IDGen.t, ~renumber_empty_holes: bool=?, UHExp.block) =>
  (UHExp.block, HTyp.t, IDGen.t);
let syn_fix_holes_lines:
  (Context.t, IDGen.t, ~renumber_empty_holes: bool=?, list(UHExp.line)) =>
  (list(UHExp.line), Context.t, IDGen.t);
let syn_fix_holes_opseq:
  (Context.t, IDGen.t, ~renumber_empty_holes: bool=?, UHExp.opseq) =>
  (UHExp.opseq, HTyp.t, IDGen.t);
let syn_fix_holes_rules:
  (Context.t, IDGen.t, ~renumber_empty_holes: bool=?, UHExp.rules, HTyp.t) =>
  (UHExp.rules, IDGen.t, list(HTyp.t), option(HTyp.t), list(Constraints.t));

/**
 * Given a pattern `e` in analytic position under context `ctx`,
 * `ana_fix_holes(ctx, id_gen, e, ty)` fixes the err statuses in `e`
 * such that it can analyze against `ty` and returns the result of
 * doing so
 */
let ana_fix_holes:
  (Context.t, IDGen.t, ~renumber_empty_holes: bool=?, UHExp.t, HTyp.t) =>
  (UHExp.t, IDGen.t);
let ana_fix_holes_block:
  (Context.t, IDGen.t, ~renumber_empty_holes: bool=?, UHExp.block, HTyp.t) =>
  (UHExp.block, IDGen.t);
let ana_fix_holes_opseq:
  (Context.t, IDGen.t, ~renumber_empty_holes: bool=?, UHExp.opseq, HTyp.t) =>
  (UHExp.opseq, IDGen.t);
let ana_fix_holes_rules:
  (
    Context.t,
    IDGen.t,
    ~renumber_empty_holes: bool=?,
    UHExp.rules,
    HTyp.t,
    HTyp.t
  ) =>
  (UHExp.rules, IDGen.t, list(Constraints.t));

let syn_fix_holes_z:
  (Context.t, IDGen.t, ZExp.t) => (ZExp.t, HTyp.t, IDGen.t);
let syn_fix_holes_zlines:
  (Context.t, IDGen.t, ZExp.zblock) => (ZExp.zblock, Context.t, IDGen.t);
let syn_fix_holes_zrules:
  (Context.t, IDGen.t, ZExp.zrules, HTyp.t) =>
  (ZExp.zrules, list(HTyp.t), option(HTyp.t), IDGen.t, list(Constraints.t));

let ana_fix_holes_z:
  (Context.t, IDGen.t, ZExp.t, HTyp.t) => (ZExp.t, IDGen.t);
let ana_fix_holes_zrules:
  (Context.t, IDGen.t, ZExp.zrules, HTyp.t, HTyp.t) =>
  (ZExp.zrules, IDGen.t, list(Constraints.t));

let fix_and_renumber_holes:
  (Context.t, UHExp.t) => (UHExp.t, HTyp.t, IDGen.t);

let fix_and_renumber_holes_z: (Context.t, ZExp.t) => Statics.edit_state;

let joined_pattern_type: (Context.t, list(UHExp.rule)) => option(HTyp.t);

/**
 * Currently we restrict recursive definitions to let lines with
 * type annotations that bind a function to a single variable.
 * Given a let line with a pattern `p`, and a defining expression `e`,
 * `extend_let_def_ctx(ctx, p, e)` returns the context available to `e`.
 */
let extend_let_def_ctx: (Context.t, UHPat.t, UHExp.t) => Context.t;

/**
 * Currently we restrict recursive definitions to let lines with
 * type annotations that bind a function to a single variable.
 * Given a let line with a pattern `p`, and a defining expression `e`,
 * `recursive_let_id(ctx, p, e)` returns the name and type of the recursive
 * reference, if any.
 */
let recursive_let_id: (Context.t, UHPat.t, UHExp.t) => option(Var.t);

/**
 * Extends the provided context, joining the type of the pattern
 * with the type of the defining expression.
 * Precondition: provided pattern and expression have consistent types
 */
let extend_let_body_ctx: (Context.t, UHPat.t, UHExp.t) => Context.t;
