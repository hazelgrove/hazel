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
let ana_skel: (Contexts.t, UHExp.skel, UHExp.seq, HTyp.t) => option(unit);

/**
 * Given a pattern `e` in synthetic position under context `ctx`,
 * `syn_fix_holes(ctx, u_gen, e)` fixes the err statuses in `e` such
 * that it can synthesize a type and returns the results of doing so
 */
let syn_fix_holes:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHExp.t) =>
  (UHExp.t, HTyp.t, MetaVarGen.t);
let syn_fix_holes_block:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHExp.block) =>
  (UHExp.block, HTyp.t, MetaVarGen.t);
let syn_fix_holes_lines:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    list(UHExp.line)
  ) =>
  (list(UHExp.line), Contexts.t, MetaVarGen.t);
let syn_fix_holes_opseq:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHExp.opseq) =>
  (UHExp.opseq, HTyp.t, MetaVarGen.t);
let syn_fix_holes_rules:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    UHExp.rules,
    HTyp.t
  ) =>
  (UHExp.rules, MetaVarGen.t, list(HTyp.t), option(HTyp.t));

/**
 * Given a pattern `e` in analytic position under context `ctx`,
 * `ana_fix_holes(ctx, u_gen, e, ty)` fixes the err statuses in `e`
 * such that it can analyze against `ty` and returns the result of
 * doing so
 */
let ana_fix_holes:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHExp.t, HTyp.t) =>
  (UHExp.t, MetaVarGen.t);
let ana_fix_holes_block:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    UHExp.block,
    HTyp.t
  ) =>
  (UHExp.block, MetaVarGen.t);
let ana_fix_holes_opseq:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    UHExp.opseq,
    HTyp.t
  ) =>
  (UHExp.opseq, MetaVarGen.t);
let ana_fix_holes_rules:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    UHExp.rules,
    HTyp.t,
    HTyp.t
  ) =>
  (UHExp.rules, MetaVarGen.t);

let syn_fix_holes_z:
  (Contexts.t, MetaVarGen.t, ZExp.t) => (ZExp.t, HTyp.t, MetaVarGen.t);
let syn_fix_holes_zlines:
  (Contexts.t, MetaVarGen.t, ZExp.zblock) =>
  (ZExp.zblock, Contexts.t, MetaVarGen.t);
let syn_fix_holes_zrules:
  (Contexts.t, MetaVarGen.t, ZExp.zrules, HTyp.t) =>
  (ZExp.zrules, list(HTyp.t), option(HTyp.t), MetaVarGen.t);

let ana_fix_holes_z:
  (Contexts.t, MetaVarGen.t, ZExp.t, HTyp.t) => (ZExp.t, MetaVarGen.t);
let ana_fix_holes_zrules:
  (Contexts.t, MetaVarGen.t, ZExp.zrules, HTyp.t, HTyp.t) =>
  (ZExp.zrules, MetaVarGen.t);

let fix_and_renumber_holes:
  (Contexts.t, UHExp.t) => (UHExp.t, HTyp.t, MetaVarGen.t);

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
