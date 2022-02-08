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

let gamma: t => VarCtx.t = ({gamma, _}) => gamma;

let palette_ctx: t => PaletteCtx.t = ({palette, _}) => palette;

let tyvars: t => TyVarCtx.t = ({tyvars, _}) => tyvars;

let bind_tyvar = (ctx: t, name: TyVar.Name.t, k: Kind.t): t => {
  let tyvars = ctx.tyvars |> TyVarCtx.bind(name, k);
  {...ctx, tyvars};
};
