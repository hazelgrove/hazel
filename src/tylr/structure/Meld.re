open Sexplib.Std;
open Util;

module Cell = {
  type t('sort, 'meld) = {
    marks: Path.Marks.t,
    sort: 'sort,
    meld: option('meld),
  };
};

module Base = {
  type t =
    | M(cell, wald, cell)
  and wald =
    | W(Chain.t(Token.t, cell))
  and cell = Cell.t(Bound.t(Molded.Sort.t), t);
};
include Base;

let mk = (~l: cell, ~r: cell, w) => M(l, w, r);

let link = (c: cell, t: Token.t, M(l, W(w), r): t) =>
  M(c, W(Chain.link(t, l, w)), r);

let rev = (M(l, W(w), r): t) => M(r, W(Chain.rev(w)), l);

// let singleton = (~l=Cell.empty, ~r=Cell.empty, t) =>
//   mk(~l, W(Chain.of_loop(t)), ~r);

// let of_piece = (~l=empty(), ~r=empty(), p: Piece.t) =>
//   of_chain(Chain.mk([l, r], [p])) |> aggregate;

// let root = mel =>
//   mel.chain |> Option.map(Chain.links) |> Option.value(~default=[]);
// let kids = mel =>
//   mel.chain |> Option.map(Chain.loops) |> Option.value(~default=[]);
// let length = mel => List.length(root(mel));

// let sort = mel => root(mel) |> ListUtil.hd_opt |> Option.map(Piece.sort);
// let prec = _ => failwith("todo prec");

// let fst_id = mel => Option.map(Piece.id_, end_piece(~side=L, mel));
// let lst_id = mel => Option.map(Piece.id_, end_piece(~side=R, mel));

// let link = (~slot=None, a, M(l, W(w), r)) =>
//   M(slot, W(Chain.link(a, l, w)), r);

// // left to right: l w r a slot
// let knil = (~slot=None, M(l, W(w), r), a) =>
//   M(l, W(Chain.knil(w, r, a)), slot);

// let unlink = (M(l, W(w), r)) =>
//   Chain.unlink(w)
//   |> Result.map(~f=((a, slot, tl)) => (l, a, (slot, W(tl), r)))
//   |> Result.map_error(~f=a => (l, a, r));
