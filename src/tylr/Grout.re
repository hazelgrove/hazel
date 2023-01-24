open Sexplib.Std;
open Util;

// [@deriving (show({with_path: false}), sexp, yojson)]
// type shape =
//   | Convex
//   | Concave(Prec.t)
//   | Molded(Mold.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  mold: Mold.t,
  prefix: string,
};

let mk = (~id=?, ~prefix="", mold) => {
  let id = id |> OptUtil.get(() => Id.Gen.next());
  {id, mold, prefix};
};

let mk_convex = (~id=?, sort: Sort.o) => mk(~id?, Mold.mk(sort, Prec.max));

let mk_concave = (~id=?, l: Mold.t, r: Mold.t) => {
  let sort = Sort.lca(l.sort, r.sort);
  let prec = min(l.prec, r.prec);
  mk(~id?, Mold.mk_infix(sort, prec));
};

// todo: incorporate unique filling
let length = _ => 1;

let suggestion = _ => failwith("todo");
