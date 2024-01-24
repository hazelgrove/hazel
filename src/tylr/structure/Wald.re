open Util;

include Meld.Wald;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Meld.Wald.t(Meld.cell);

let mk = (toks: list(_), cells: list(Cell.t)) => W(Chain.mk(toks, cells));
let unit = tok => mk([tok], []);

let split_hd = (W(w)) => Chain.split_fst(w);

// let fst = (W(w)) => Chain.fst(w);
// let lst = (W(w)) => Chain.lst(w);
// let face =
//   fun
//   | Dir.L => fst
//   | R => lst;

let map = (f, W(w)) => W(f(w));
let link = (tok, cell) => map(Chain.link(tok, cell));

let merge = (~from: Dir.t, src: t, dst: t): option(t) => {
  let (hd_src, tl_src) = split_hd(src);
  let (hd_dst, tl_dst) = split_hd(dst);
  let (hd_l, hd_r) = Dir.order(from, (hd_src, hd_dst));
  Token.zip(hd_l, hd_r)
  |> Option.map(tok => W(Chain.zip(tl_dst, tok, tl_src)));
};
