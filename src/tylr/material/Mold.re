// include PBNF.Ctx;
// type t = PBNF.Ctx.t(MSym.t);

type t = {
  sort: Mtrl.Sort.t,
  prec: Prec.t,
  rctx: RCtx.t(Mtrl.Sym.t),
};
