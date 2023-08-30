open Sexplib.Std;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('a) = list(Terrace.t('a));
  [@deriving (show({with_path: false}), sexp, yojson)]
  type m = t(Material.Molded.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type p = t(Piece.t);
};
include Base;

let empty = [];
let singleton = t => [t];
// let of_piece = p => of_terr(Terrace.of_piece(p));
let height = List.length;

let fold = List.fold_left;

// Dn and Up slopes named based on left-to-right order of terraces
// as they appear in edit state, but terraces are always maintained
// internally low-to-high

module Dn = {
  include Base;

  let of_slot = Terrace.R.s_of_slot;

  let to_slot =
    fold(
      (slot, t: Terrace.p) => Slot.full(M(t.slot, t.wald, slot)),
      Slot.empty,
    );

  let pull = (~char=false, dn: p): option((p, Piece.t)) =>
    switch (dn) {
    | [] => None
    | [hd, ...tl] =>
      let (rest, p) = Terrace.R.pull(~char, hd);
      Some((rest @ tl, p));
    };
};

module Up = {
  include Base;

  let of_slot = Terrace.L.s_of_slot;

  let to_slot =
    fold(
      (slot, t: Terrace.p) => Slot.full(M(slot, t.wald, t.slot)),
      Slot.empty,
    );

  let pull = (~char=false, up: p): option((Piece.t, p)) =>
    switch (up) {
    | [] => None
    | [hd, ...tl] =>
      let (p, rest) = Terrace.L.pull(~char, hd);
      Some((p, rest @ tl));
    };
};
