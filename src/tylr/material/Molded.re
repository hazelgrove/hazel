module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('mtrl) = {
    mtrl: 'mtrl,
    mold: Mold.t,
  };
};
include Base;

let mtrl_ = (m: t(_)) => m.mtrl;

let get = (f, m) => f(m.mtrl);
let map = (f, m) => {...m, mtrl: f(m)};

let nullable = (~side: Dir.t, m: t(_)) => Mold.nullable(~side, m.mold);

module Labeled = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Mtrl.Labeled.t);

  let space = {
    mtrl: Mtrl.Space,
    mold: {
      sort: Space,
      prec: 0,
      rctx: [Seq_([Atom(NT(Space))], [Atom(NT(Space))])],
    },
  };

  let mk_grout = (~l=false, ~r=false, sort: Mtrl.Sorted.t) => {
    let rctx: RCtx.t(Mtrl.Sym.t) =
      if (l && r) {
        [Seq_([Atom(NT(Grout))], [Atom(NT(Grout))])];
      } else if (l) {
        [Seq_([Atom(NT(Grout))], [])];
      } else if (r) {
        [Seq_([], [Atom(NT(Grout))])];
      } else {
        [];
      };
    let mold = Mold.{sort, prec: 0, rctx};
    {mold, mtrl: Mtrl.Grout};
  };
};

module Sorted = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Padded.t(Mtrl.Sorted.t));
  let bounds = _ => failwith("todo Molded.Sorted.bounds");
  // let indent = get(Mtrl.PSort.indent);
};
