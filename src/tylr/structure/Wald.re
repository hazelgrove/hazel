open Util;

include Meld.Base;

type t = Meld.wald;

let mk = (toks: list(_), cells: list(Cell.t(_))) =>
  W(Chain.mk(toks, cells));
let singleton = tok => mk([tok], []);

let split_hd = (W(w)) => Chain.split_fst(w);

// let fst = (W(w)) => Chain.fst(w);
// let lst = (W(w)) => Chain.lst(w);
// let face =
//   fun
//   | Dir.L => fst
//   | R => lst;

let link = (t, ~cell=Cell.empty, W(w)) => W(Chain.link(t, cell, w));
let knil = (W(w), ~cell=Cell.empty, t) => W(Chain.knil(w, cell, t));

let append = (W(l): t(_), ~cell=Cell.empty, r: t(_)) =>
  l
  |> Chain.fold_right(
       (tok, cell) => link(tok, ~cell),
       tok => link(tok, ~cell, r),
     );

let zip = (~from: Dir.t, src: Wald.t, dst: Wald.t): option(Wald.t) => {
  let (hd_src, tl_src) = split_hd(src);
  let (hd_dst, tl_dst) = split_hd(dst);
  let (hd_l, hd_r) = Dir.order(from, hd_src, hd_dst);
  Token.zip(hd_l, hd_r) |> Option.map(t => W(Chain.zip(tl_dst, t, tl_src)));
};
