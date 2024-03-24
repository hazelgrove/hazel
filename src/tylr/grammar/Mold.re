[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = {
  sort: Mtrl.Sorted.t,
  prec: Prec.t,
  rctx: RCtx.t(Mtrl.Sym.t),
};

module Space = {
  let t = Sym.T(Mtrl.Space);
  let nt = Sym.NT(Mtrl.NT.space);
  let mk = (rctx: RCtx.t(_)) => {sort: Space, prec: 0, rctx};
  let of_t = mk([Seq_([Atom(nt)], [Atom(nt)])]);
  let of_nt =
    fun
    | Dir.L => mk([RFrame.Seq_([], [Atom(t), Atom(nt)])])
    | R => mk([RFrame.Seq_([Atom(nt), Atom(t)], [])]);
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
