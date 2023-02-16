open Util;

type t = Chain.t(Piece.t, Meld.t);

let of_piece: _ => t = Chain.of_loop;
let of_meld = (mel: Meld.t): option((Meld.t, t, Meld.t)) =>
  Option.bind(Meld.distribute(mel).chain, Chain.trim);

let to_meld = (~l=Meld.empty(), ~r=Meld.empty(), ret: t) =>
  Chain.untrim(l, ret, r);

// let of_l = ({hd, tl}: Terrace.L.t, p: Piece.t) => Chain.untrim(hd, tl, p);
// let of_r = (p: Piece.t, {tl, hd}: Terrace.R.t) => Chain.untrim(p, tl, hd);

// let to_l = (kid: Meld.t, c: t)
