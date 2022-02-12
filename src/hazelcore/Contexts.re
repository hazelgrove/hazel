[@deriving sexp]
type t = (VarCtx.t, PaletteCtx.t, TyVarCtx.t);
let empty = (VarCtx.empty, PaletteCtx.empty, TyVarCtx.empty);

let gamma = ((gamma, _, _): t): VarCtx.t => gamma;
let extend_gamma = (ctx: t, binding: (Var.t, HTyp.t)): t => {
  let (gamma, palette_ctx, tyvars) = ctx;
  let gamma' = VarCtx.extend(gamma, binding);
  (gamma', palette_ctx, tyvars);
};

let palette_ctx = ((_, palette_ctx, _): t): PaletteCtx.t => palette_ctx;

let tyvar_ctx = ((_, _, tyvar_ctx): t): TyVarCtx.t => tyvar_ctx;
let extend_tyvar_ctx =
    ((gamma, palette_ctx, tyvar_ctx): t, name: string, kind: Kind.t) => {
  (gamma, palette_ctx, TyVarCtx.bind(tyvar_ctx, name, kind));
};
