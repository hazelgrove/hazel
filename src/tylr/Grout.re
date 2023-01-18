open Sexplib.Std;

// [@deriving (show({with_path: false}), sexp, yojson)]
// type shape =
//   | Convex
//   | Concave(Prec.t)
//   | Molded(Mold.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  // unmolded case for concave grout wrapping
  mold: option(Mold.t),
};

let mk = (~id=?, ~mold=?, ()) => {
  let id =
    switch (id) {
    | None => Id.Gen.next()
    | Some(id) => id
    };
  {id, mold};
};

let mk_convex = (~id=?, sort: Sort.t) =>
  mk(~id?, ~mold=Mold.mk(sort, Prec.max), ());

let mk_concave = (~id=?, l: option(Mold.t), r: option(Mold.t)) => {
  let mold =
    switch (l, r) {
    | (None, None) => None
    | (None, Some(m))
    | (Some(m), None) =>
      Some(Mold.mk_infix(~l=m.sort, ~r=m.sort, m.sort, m.prec))
    | (Some(l), Some(r)) =>
      let s = Sort.lca(l.sort, r.sort);
      let p = min(l.prec, r.prec);
      Some(Mold.mk_infix(~l=s, ~r=s, s, p));
    };
  mk(~id?, ~mold?, ());
};

// todo: incorporate unique filling
let length = _ => 1;
