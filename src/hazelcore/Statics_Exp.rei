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
  (Contexts.t, UHPat.t, HTyp.t, UHExp.t) => (Contexts.t, option(Var.t));

/**
 * `syn_nth_type_mode(ctx, n, opseq)` returns the type mode of the
 * `n`th operand of `opseq`, assuming `opseq` is in synthetic position
 * under context `ctx`. Returns `None` if `n` is not a valid operand
 * index for `opseq`.
 */
let syn_nth_type_mode:
  (Contexts.t, int, UHExp.opseq) => option(Statics_common.type_mode);
/**
 * `ana_nth_type_mode(ctx, n, opseq, ty)` returns the type mode of the
 * `n`th operand of `opseq`, assuming `opseq` is in analytic position
 * against type `ty` under context `ctx`. Returns `None` if `n` is not
 * a valid operand index for `opseq`.
 */
let ana_nth_type_mode:
  (Contexts.t, int, UHExp.opseq, HTyp.t) => option(Statics_common.type_mode);

/**
 * `syn(ctx, e)` synthesizes a type for `e` under context `ctx`
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
 * `ana(ctx, p, ty)` analyzes `e` against `ty` under context `ctx`
 */
let ana: (Contexts.t, UHExp.t, HTyp.t) => option(unit);
let ana_skel: (Contexts.t, UHExp.skel, UHExp.seq, HTyp.t) => option(unit);
let ana_splice_map: (Contexts.t, UHExp.splice_map) => option(Contexts.t);

/**
 * `syn_fix_holes(ctx, u_gen, e)` fixes the err statuses in `e` such
 * that it can synthesize a type under context `ctx` and and returns
 * the results of doing so
 */
let syn_fix_holes:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHExp.t) =>
  (UHExp.t, HTyp.t, MetaVarGen.t);
let syn_fix_holes_block:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHExp.block) =>
  (UHExp.block, HTyp.t, MetaVarGen.t);
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
 * `ana_fix_holes(ctx, u_gen, e, ty)` fixes the err statuses in `e`
 * such that it can successfully analyze against `ty` under context
 * `ctx` and returns the result of doing so
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

let fix_and_renumber_holes:
  (Contexts.t, UHExp.t) => (UHExp.t, HTyp.t, MetaVarGen.t);

let fix_and_renumber_holes_z:
  (Contexts.t, ZExp.t) => Statics_common.edit_state;
