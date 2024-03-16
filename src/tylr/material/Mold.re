[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = {
  sort: Mtrl.Sorted.t,
  prec: Prec.t,
  rctx: RCtx.t(Mtrl.Sym.t),
};

let equal = (==);

let push = (~onto: Dir.t, msym: Mtrl.Sym.t, mold: t) => {
  ...mold,
  rctx: RCtx.push(~onto, Atom(msym), mold.rctx),
};

let nullable = (~side: Dir.t, m: t) => RCtx.nullable(side, m.rctx);

let of_grout = (~l=false, ~r=false, sort: Mtrl.Sorted.t) => {
  let rctx: RCtx.t(Mtrl.Sym.t) =
    if (l && r) {
      [Seq_([Atom(NT(Mtrl.NT.grout))], [Atom(NT(Mtrl.NT.grout))])];
    } else if (l) {
      [Seq_([Atom(NT(Mtrl.NT.grout))], [])];
    } else if (r) {
      [Seq_([], [Atom(NT(Mtrl.NT.grout))])];
    } else {
      [];
    };
  {sort, prec: 0, rctx};
};
