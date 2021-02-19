[@deriving sexp]
type t = {
  gamma: VarCtx.t,
  palette: PaletteCtx.t,
  tyvars: TyVarCtx.t,
};
let empty = {
  gamma: VarCtx.empty,
  palette: PaletteCtx.empty,
  tyvars: TyVarCtx.empty,
};

let extend_gamma = (ctx: t, binding: (Var.t, HTyp.t)): t => {
  let gamma' = VarCtx.extend(ctx.gamma, binding);
  {...ctx, gamma: gamma'};
};

let gamma: t => VarCtx.t = ({tyvars: _, gamma, palette: _}) => gamma;

let palette_ctx: t => PaletteCtx.t =
  ({tyvars: _, gamma: _, palette}) => palette;

let tyvars: t => TyVarCtx.t = ({tyvars, gamma: _, palette: _}) => tyvars;
