let tuple_zip: (UHPat.skel, HTyp.t) => option(list((UHPat.skel, HTyp.t)));

/**
 * `syn_nth_type_mode(ctx, n, opseq)` returns the type mode of the `n`th
 * operand of `opseq`, assuming `opseq` is in synthetic position under
 * context `ctx`. Returns `None` if `n` is not a valid operand index
 * for `opseq`.
 */
let syn_nth_type_mode:
  (Contexts.t, int, UHPat.opseq) => option(Statics_common.type_mode);
/**
 * `ana_nth_type_mode(ctx, n, opseq, ty)` returns the type mode of the `n`th
 * operand of `opseq`, assuming `opseq` is in analytic position against type
 * `ty` under context `ctx`. Returns `None` if `n` is not a valid operand index
 * for `opseq`.
 */
let ana_nth_type_mode:
  (Contexts.t, int, UHPat.opseq, HTyp.t) => option(Statics_common.type_mode);

/**
 * `syn(ctx, p)` synthesizes a type for `p` under context `ctx` and
 * produces a new context with bindings introduced by `p` (if possible)
 */
let syn: (Contexts.t, UHPat.t) => option((HTyp.t, Contexts.t));
let syn_opseq: (Contexts.t, UHPat.opseq) => option((HTyp.t, Contexts.t));
let syn_skel:
  (Contexts.t, UHPat.skel, UHPat.seq) => option((HTyp.t, Contexts.t));
let syn_operand: (Contexts.t, UHPat.operand) => option((HTyp.t, Contexts.t));

/**
 * `ana(ctx, p, ty)` analyzes `p` against `ty` under context `ctx` and
 * produces a new context with bindings introduced by `p` if successful
 */
let ana: (Contexts.t, UHPat.t, HTyp.t) => option(Contexts.t);
let ana_skel:
  (Contexts.t, UHPat.skel, UHPat.seq, HTyp.t) => option(Contexts.t);

/**
 * `syn_fix_holes(ctx, u_gen, p)` fixes the err statuses in `p` such
 * that it can synthesize a type under context `ctx` and returns the
 * results of doing so
 */
let syn_fix_holes:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHPat.t) =>
  (UHPat.t, HTyp.t, Contexts.t, MetaVarGen.t);

/**
 * `ana_fix_holes(ctx, u_gen, p, ty)` fixes the err statuses in `p`
 * such that it can successfully analyze against `ty` under context
 * `ctx` and returns the result of doing so
 */
let ana_fix_holes:
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHPat.t, HTyp.t) =>
  (UHPat.t, Contexts.t, MetaVarGen.t);
let ana_fix_holes_opseq:
  (
    Contexts.t,
    MetaVarGen.t,
    ~renumber_empty_holes: bool=?,
    UHPat.opseq,
    HTyp.t
  ) =>
  (UHPat.opseq, Contexts.t, MetaVarGen.t);

/**
 * The ticked versions of `[syn|ana]_fix_holes` have the same behavior
 * as the unticked versions, the only difference being that they return
 * an additional `bool` indicating whether any err statuses were updated.
 * For internal use only within `Statics_*` modules to ensure pointer
 * stability when there are no updates.
 */
let syn_fix_holes':
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHPat.t) =>
  (UHPat.t, HTyp.t, Contexts.t, MetaVarGen.t, bool);
let ana_fix_holes':
  (Contexts.t, MetaVarGen.t, ~renumber_empty_holes: bool=?, UHPat.t, HTyp.t) =>
  (UHPat.t, Contexts.t, MetaVarGen.t, bool);

let syn_fix_holes_z:
  (Contexts.t, MetaVarGen.t, ZPat.t) =>
  (ZPat.t, HTyp.t, Contexts.t, MetaVarGen.t);

let ana_fix_holes_z:
  (Contexts.t, MetaVarGen.t, ZPat.t, HTyp.t) =>
  (ZPat.t, Contexts.t, MetaVarGen.t);
