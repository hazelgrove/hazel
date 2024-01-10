// include PBNF.Ctx;
// type t = PBNF.Ctx.t(MSym.t);

type t = {
  sort: Mtrl.Sort.t,
  prec: Prec.t,
  rctx: RCtx.t(Mtrl.Sym.t),
};

let push = (~onto: Dir.t, msym: Mtrl.Sym.t, mold: t) =>
  {...mold, rctx: RCtx.push(~onto, Atom(msym), mold)};