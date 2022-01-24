// [@deriving sexp]
type t = {
  gamma: VarCtx.t,
  palette: PaletteCtx.t,
  typing: TyCtx.t,
};
let empty = {
  gamma: VarCtx.empty,
  palette: PaletteCtx.empty,
  typing: TyCtx.empty,
};

let extend_gamma = (ctx: t, binding: (Var.t, HTyp.t)): t => {
  let gamma' = VarCtx.extend(ctx.gamma, binding);
  {...ctx, gamma: gamma'};
};

let gamma: t => VarCtx.t = ({gamma, _}) => gamma;

let palette_ctx: t => PaletteCtx.t = ({palette, _}) => palette;

let typing: t => TyCtx.t = ({typing, _}) => typing;

let bind_tyvar = (name: TyVar.Name.t, k: Kind.t(HTyp.t), ctx: t): t => {
  let increment_singleton: TyCtx.Vars.binding => TyCtx.Vars.binding =
    fun
    | (name, Singleton(k', ty)) => {
        let k = Kind.Singleton(k', HTyp.increment_indices(ty));
        (name, k);
      }
    | binding => binding;
  let typing = ctx.typing |> TyCtx.bind_var(name, k, ~increment_singleton);
  {...ctx, typing};
};

// let extend_tyvars = (t: TyVar.Name.t, k: Kind.t, ctx: t): t => {
//   let increment_singleton: TyCtx.Vars.binding => TyCtx.Vars.binding =
//     fun
//     | (t', Singleton(k', ty)) => {
//         let k = Kind.Singleton(k', HTyp.increment_indices(ty));
//         (t', k);
//       }
//     | binding => binding;
//   let vars = ctx.tyctx.vars |> TyCtx.Vars.extend(t, k, ~increment_singleton);
//   let tyctx = {...ctx.tyctx, vars};
//   {...ctx, tyctx};
// };

// let tyholes = ({tyholes, _}: t): TyVar.HoleCtx.t => tyholes;

// let extend_tyholes = (ctx: t, u: MetaVar.t, k: Kind.t): t => {
//   ...ctx,
//   tyholes: ctx.tyholes |> TyVar.HoleCtx.add(u, k),
// };
