// molded mtrlized syms
module T = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = (Mtrl.T.t, Mold.t);
  let mtrl = ((mtrl, _): t) => mtrl;
  let padding = ((mtrl, _): t) => Mtrl.Labeled.padding(mtrl);
};
module NT = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = (Mtrl.NT.t, Mold.t);
  let mtrl = fst;
  let bounds = _ => failwith("todo");
};
module Sym = {
  include Sym;
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Sym.t(T.t, NT.t);
};
