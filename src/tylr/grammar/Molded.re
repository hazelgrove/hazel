// molded mtrlized syms
module T = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = (Mtrl.T.t, Mold.t);
};
module NT = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = (Mtrl.NT.t, Mold.t);
  let padding = (((p, _), _): t) => p;
  let mtrl = (((_, mtrl), _): t) => mtrl;
  let bounds = _ => failwith("todo");
};
module Sym = {
  include Sym;
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Sym.t(T.t, NT.t);
};
