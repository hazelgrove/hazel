[@deriving (sexp, show)]
type t = (VarCtx.t, PaletteCtx.t);
let empty = (VarCtx.empty, PaletteCtx.empty);

let gamma = ((gamma, _): t): VarCtx.t => gamma;
let extend_gamma = (ctx: t, binding: (Var.t, HTyp.t)): t => {
  let (gamma, palette_ctx) = ctx;
  let gamma' = VarCtx.extend(gamma, binding);
  (gamma', palette_ctx);
};

let palette_ctx = ((_, palette_ctx): t): PaletteCtx.t => palette_ctx;
