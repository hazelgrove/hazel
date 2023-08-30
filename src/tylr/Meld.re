open Sexplib.Std;
open Util;

// for constructor inclusion in Wald and Slot modules
module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('a) =
    | M(slot('a), wald('a), slot('a))
  and wald('a) =
    | W(Chain.t('a, slot('a)))
  and slot('a) = option(t('a));
};
include Base;

module Profile = {
  type t = {
    has_tokens: bool,
    sort: Material.sorted,
  };
};

// we expect kids to have higher precedence than their
// parent tips (which may be min prec in bidelim containers)
exception Invalid_prec;

let mk = (~l=None, ~r=None, w) => M(l, w, r);

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
