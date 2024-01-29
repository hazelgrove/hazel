open Util;

include Meld.Wald;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Meld.Wald.t(Cell.t);

let mk = (toks: list(_), cells: list(Cell.t)) => W(Chain.mk(toks, cells));
let unit = tok => mk([tok], []);

let split_hd = (W(w)) => Chain.split_fst(w);
let hd = w => fst(split_hd(w));

// let fst = (W(w)) => Chain.fst(w);
// let lst = (W(w)) => Chain.lst(w);
// let face =
//   fun
//   | Dir.L => fst
//   | R => lst;

let map = (f, W(w)) => W(f(w));
let rev: t => t = map(Chain.rev);
let link = (tok, cell) => map(Chain.link(tok, cell));
let unlink = (W(w)) =>
  Chain.unlink(w)
  |> Result.map(~f=((tok, cell, tl)) => (tok, cell, W(tl)));

let face = (~side=Dir.L, W((toks, _)): t) =>
  Dir.pick(side, (List.hd, ListUtil.last), toks).lbl;

let sort = w => Token.sort(hd(w));

let merge = (~from: Dir.t, src: t, dst: t): option(t) => {
  let (hd_src, tl_src) = split_hd(src);
  let (hd_dst, tl_dst) = split_hd(dst);
  let (hd_l, hd_r) = Dir.order(from, (hd_src, hd_dst));
  Token.zip(hd_l, hd_r)
  |> Option.map(tok => W(Chain.zip(~pre=tl_dst, tok, ~suf=tl_src)));
};
