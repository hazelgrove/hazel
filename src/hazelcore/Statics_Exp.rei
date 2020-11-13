let tuple_zip: (UHExp.skel, HTyp.t) => option(list((UHExp.skel, HTyp.t)));

/**
 * Currently we restrict recursive definitions to let lines with
 * type annotations that bind a function to a single variable.
 * Given a let line with a pattern `p`, type annotation `ty`,
 * and a defining expression `e`, `ctx_for_let(ctx, p, ty, e)`
 * returns the context available to `e`. If the let line satifies
 * our conditions for recursion, then this function also returns
 * the recursively defined variable.
 */
let ctx_for_let:
  (Contexts.t'('a), UHPat.t, HTyp.t, UHExp.t) =>
  (Contexts.t'('a), option(Var.t));

type livelit_types_type = {
  init_ty: HTyp.t,
  update_ty: HTyp.t,
  view_ty: HTyp.t,
  shape_ty: HTyp.t,
  expand_ty: HTyp.t,
};
/**
 * Determine member function types for livelit def `llrecord`
 */
let livelit_types: UHExp.livelit_record => livelit_types_type;

/**
 * Get type mode of nth operand of an opseq in synthetic position
 */
let syn_nth_type_mode:
  (Contexts.t, int, UHExp.opseq) => option(Statics_common.type_mode);
/**
 * Get type mode of nth operand of an opseq in analytic position
 */
let ana_nth_type_mode:
  (Contexts.t, int, UHExp.opseq, HTyp.t) => option(Statics_common.type_mode);

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
let ana_fix_holes_splice_map:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    UHExp.splice_map
  ) =>
  (UHExp.splice_map, MetaVarGen.t);

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

let fix_and_renumber_holes:
  (Contexts.t, UHExp.t) => (UHExp.t, HTyp.t, MetaVarGen.t);

let fix_and_renumber_holes_z:
  (Contexts.t, ZExp.t) => Statics_common.edit_state;
