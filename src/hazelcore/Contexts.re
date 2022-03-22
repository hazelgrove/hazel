[@deriving sexp]
type t = (VarCtx.t, VarCtx.t, TyVarCtx.t);
let initial: t = (VarCtx.empty, VarCtx.empty, TyVarCtx.empty);

let gamma = ((gamma, _, _): t): VarCtx.t => gamma;
let extend_gamma = (ctx: t, binding: (Var.t, HTyp.t)): t => {
  let (gamma, builtins, tyvars) = ctx;
  let gamma' = VarCtx.extend(gamma, binding);
  (gamma', builtins, tyvars);
};

let tyvars = ((_, _, tyvars): t): TyVarCtx.t => tyvars;
let extend_tyvars =
    ((gamma, builtins, tyvars): t, name: string, kind: Kind.t) => {
  (gamma, builtins, TyVarCtx.bind(tyvars, name, kind));
};
let unbind0 = ((gamma, builtins, tyvars): t): t => {
  let kind = TyVarCtx.kind(tyvars, 0) |> Option.value(~default=Kind.KHole);
  let gamma = VarCtx.subst_tyvar(gamma, 0, Kind.canonical_type(kind));
  (gamma, builtins, TyVarCtx.unbind0(tyvars));
};
