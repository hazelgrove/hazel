// open Util;

// https://en.wikipedia.org/wiki/Prototile
// tiles and grout are instances of prototiles
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = {
  mold: Mold.t,
  label: Label.t,
};

let mold_ = p => p.mold;
let label_ = p => p.label;

// let length = p => Token.length(p.label);

// let uncons_char = (p: t): option((t, t)) => {
//   open OptUtil.Syntax;
//   let* (hd, tl) = StringUtil.uncons(p.label);
//   tl == "" ? None : Some(({...p, label: hd}, {...p, label: tl}));
// };
// let unsnoc_char = (p: t): option((t, t)) => {
//   open OptUtil.Syntax;
//   let* (tl, hd) = StringUtil.unsnoc(p.label);
//   tl == "" ? None : Some(({...p, label: tl}, {...p, label: hd}));
// };

// // assumes client checked for zippability (eg same mold)
// // todo: rename to avoid clashing with grammar zipper
// let zip = (l: t, r: t): t => {...l, label: l.label ++ r.label};
// let unzip = (n: int, p: t): Either.t(Dir.t, (t, t)) =>
//   Token.unzip(n, p.label)
//   |> Either.map_r(((l, r)) => ({...p, label: l}, {...p, label: r}));

// module Tile = {
//   let mk = (mold, label) => {mold, label};
// };

// module Grout = {
//   let mk = mold => {mold, label: ""};
//   // let mk_operand = s => mk(Mold.mk_operand(s));
//   // let mk_prefix = (~r=?, s, p) =>
//   //   mk(Mold.mk_prefix(~r?, s, p));
//   // let mk_postfix = (~l=?, s, p) =>
//   //   mk(Mold.mk_postfix(~l?, s, p));
//   // let mk_infix = (~l=?, ~r=?, s, p) =>
//   //   mk(Mold.mk_infix(~l?, ~r?, s, p));
// };

// module Ord = {
//   type nonrec t = t;
//   let compare = compare;
// };
// module Map = Map.Make(Ord);
// module Set = Set.Make(Ord);
