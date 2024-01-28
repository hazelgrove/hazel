module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('mtrl) = {
    mtrl: 'mtrl,
    mold: Mold.t,
  };
};
include Base;

let map = (f, m) => {...m, mtrl: f(m)};

module Label = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Mtrl.Label.t);

  let space = {
    mtrl: Mtrl.Space,
    mold: {
      sort: Space,
      prec: 0,
      rctx: [Seq_([Atom(NT(Space))], [Atom(NT(Space))])],
    },
  };

  let mk_grout = (~l=false, ~r=false, sort: Mtrl.Sort.t) => {
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

module Sort = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Mtrl.Sort.t);
  let bounds = _ => failwith("todo Molded.Sort.bounds");
};
