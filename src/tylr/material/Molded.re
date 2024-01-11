module Base = {
  type t('mtrl) = {
    mtrl: 'mtrl,
    mold: Mold.t,
  };
};
include Base;

let map = (f, m) => {...m, mtrl: f(m)};

module Label = {
  type t = Base.t(Mtrl.Label.t);
};

module Sort = {
  type t = Base.t(Mtrl.Sort.t);
};
module Bounds = {
  type t =
    | Lt(Bound.t(Sort.t), Sort.t)
    | Eq(Sort.t)
    | Gt(Sort.t, Bound.t(Sort.t));
};
// module Sym = {
//   // maybe this is better
//   type t = Sym.t(Label.t, Sort.t);

//   type t = Base.t(Mtrl.Sym.t);
//   let t = map(Sym.t);
//   let nt = map(Sym.nt);

//   let is_sort = ({mtrl, mold}: t): option(Sort.t) =>
//     Sym.is_nt(mtrl)
//     |> Option.map(s => {mtrl: s, mold});

//   let either = ({mtrl, mold}: t): Sym.t(Label.t, Sort.t) =>
//     switch (mtrl) {
//     | T(mlbl) => T({mtrl: mlbl, mold})
//     | NT(msrt) => NT({mtrl: msrt, mold})
//     };
// };
