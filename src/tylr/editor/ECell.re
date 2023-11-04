include EMeld.Cell;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = EMeld.Cell.t(EMeld.t);

let bake = (cell: GCell.t) =>
  switch (cell) {
  | Space => empty
  | Grout() | Tile(_) =>
    mk(EMeld.singleton(EToken.mk(Grout((Convex, Convex)))))
  };

// let clear =
//   fun
//   | Empty => []
//   | Full((_, M(l, w, r))) => clear(l) @ clear_wald(w) @ clear(r)
// and clear_wald = w |> Chain.to_list(Piece.clear, clear) |> List.concat;

let has_no_tiles =
  fun
  | Empty => Some("")
  | Full(m) => EMeld.has_no_tiles(m);

module Sorted = {
  type t = Slot.t(EMaterial.Sorted.t);

  let consistent = (e: t, g: GSlot.Sorted.t) =>
    switch (e, g) {
    | (Empty, _) => true
    | (Full(_), Empty) => false
    | (Full(e), Full(g)) => EMaterial.Sorted.consistent(e, g)
    };
};

let sort: t => Sorted.t = map(EMeld.sort);

let consistent = (e: t, g: GSlot.t) =>
  Sorted.consistent(sort(e), GSlot.sort(g));

let bake: GSlot.t => t = Slot.map(_ => EMeld.mk_hole());
