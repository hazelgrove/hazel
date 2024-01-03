open Util;

include Meld.Base;

type t = Meld.wald;

let mk = (toks: list(_), cells: list(Cell.t(_))) =>
  W(Chain.mk(toks, cells));
let singleton = tok => mk([tok], []);

let split_fst = (W(w)) => Chain.split_fst(w);
let split_lst = (W(w)) => Chain.split_lst(w);

let fst = (W(w)) => Chain.fst(w);
let lst = (W(w)) => Chain.lst(w);
let face =
  fun
  | Dir.L => fst
  | R => lst;

let link = (t, ~cell=Cell.empty, W(w)) => W(Chain.link(t, cell, w));
let knil = (W(w), ~cell=Cell.empty, t) => W(Chain.knil(w, cell, t));

let append = (W(l): t(_), ~cell=Cell.empty, r: t(_)) =>
  l
  |> Chain.fold_right(
       (tok, cell) => link(tok, ~cell),
       tok => link(tok, ~cell, r),
     );
