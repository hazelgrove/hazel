open Util;

// https://en.wikipedia.org/wiki/Prototile
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  mold: Mold.t,
  label: Token.t,
};

let uncons_char = (p: t): option((t, t)) => {
  open OptUtil.Syntax;
  let* (hd, tl) = StringUtil.uncons(p.label);
  tl == "" ? None : Some(({...p, label: hd}, {...p, label: tl}));
};
let unsnoc_char = (p: t): option((t, t)) => {
  open OptUtil.Syntax;
  let* (tl, hd) = StringUtil.unsnoc(p.label);
  tl == "" ? None : Some(({...p, label: tl}, {...p, label: hd}));
};

let zip = (l: t, r: t): t => {...l, label: l.label ++ r.label};

module Tile = {
  let mk = (mold, label) => {mold, label};
};

module Grout = {
  let mk = mold => {mold, label: ""};
  // let mk_operand = s => mk(Mold.mk_operand(s));
  // let mk_prefix = (~r=?, s, p) =>
  //   mk(Mold.mk_prefix(~r?, s, p));
  // let mk_postfix = (~l=?, s, p) =>
  //   mk(Mold.mk_postfix(~l?, s, p));
  // let mk_infix = (~l=?, ~r=?, s, p) =>
  //   mk(Mold.mk_infix(~l?, ~r?, s, p));
};
