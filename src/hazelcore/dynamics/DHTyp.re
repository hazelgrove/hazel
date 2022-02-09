[@deriving sexp]
type t = (HTyp.t, TyVarCtx.t);

let lift = (ctx: TyVarCtx.t, ty: HTyp.t): t => (ty, ctx);
let type_ = ((ty, _): t): HTyp.t => ty;
let context = ((_, ctx): t): TyVarCtx.t => ctx;
