open Sexplib.Std;
open Util;

module Slot = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('a) =
    | Empty
    | Full('a);
};

module Wald = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('piece, 'slotted) =
    | W(Chain.t('piece, Slot.t('slotted)));
};

module Base = {
  type t('slotted, 'piece) =
    | M(Slot.t('slotted), Wald.t('piece, 'slotted), Slot.t('slotted));
};
include Base;

module Molded = {
  type t = Base.t(option(Sort.t), Mold.t);
};

module Baked = {
  type t = Base.t(EPath.Marked.t(t), Piece.t);
};

module Profile = {
  type t = {
    has_tokens: bool,
    sort: Material.Sorted.t,
  };
};

// we expect kids to have higher precedence than their
// parent tips (which may be min prec in bidelim containers)
exception Invalid_prec;

let mk = (~l=Slot.Empty, ~r=Slot.Empty, w) => M(l, w, r);

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

let link = (~slot=None, a, M(l, W(w), r)) =>
  M(slot, W(Chain.link(a, l, w)), r);

// left to right: l w r a slot
let knil = (~slot=None, M(l, W(w), r), a) =>
  M(l, W(Chain.knil(w, r, a)), slot);

// let unlink = (M(l, W(w), r)) =>
//   Chain.unlink(w)
//   |> Result.map(~f=((a, slot, tl)) => (l, a, (slot, W(tl), r)))
//   |> Result.map_error(~f=a => (l, a, r));
