open Util;
// include Chain;
include Meld.Base;

// "walled" meld, wario to meld's mario
[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = Meld.wald('a);
[@deriving (show({with_path: false}), sexp, yojson)]
type m = t(Material.Molded.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type p = t(Piece.t);

let singleton = a => W(Chain.of_loop(a));
// let mk = (mel: Meld.t): option((Meld.t, t, Meld.t)) =>
//   Option.bind(Meld.distribute(mel).chain, Chain.trim);

let combine = (ps: list(_), slots: list(Slot.t(_))) =>
  W(Chain.mk(ps, slots));

let split_fst = (W(w)) => Chain.split_fst(w);
let split_lst = (W(w)) => Chain.split_lst(w);

// let unmk = (~l=Meld.empty(), ~r=Meld.empty(), wal: t) =>
//   Meld.of_chain(Chain.untrim(l, wal, r)) |> Meld.aggregate;

// let join = (_, _, _) => failwith("todo Wald.join");

let map = (f, W(w)) => W(f(w));

// let sort = wal => Piece.sort(Chain.fst(wal));
// let prec = wal => Piece.prec(Chain.fst(wal));

let fst = w => map(Chain.fst, w);
let lst = w => map(Chain.lst, w);
let face =
  fun
  | Dir.L => fst
  | R => lst;

let link = (p, ~slot=Slot.empty) => map(Chain.link(p, slot));
let knil = (W(w), ~slot=Slot.empty, p) => W(Chain.knil(w, slot, p));

let append = (W(l): t(_), ~slot=Slot.empty, r: t(_)) =>
  l
  |> Chain.fold_right((p, slot) => link(p, ~slot), p => link(p, ~slot, r));
