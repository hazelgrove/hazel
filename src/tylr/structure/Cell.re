include Meld.Base;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Meld.Base.cell;

let bake = (s: ESort.t) =>
  switch (s) {
  | Space => Empty
  | Grout ()
  | Tile(_) => Full(EMeld.singleton(EToken.grout((Convex, Convex))))
  };

// returns a cell carrying the input meld repaired for sort consistency,
// provided that such repair is possible. otherwise return empty cell.
let try_fill = (m: EMeld.Marked.t, s: ESort.t) =>
  switch (EMeld.sorted(snd(m)), s) {
  | (Space, _) => Full(m)
  | (_, Space) => Empty
  | (Grout((s, (l, r))), Grout(s')) =>
    // check to see if it's possible to unwrap grout
    // if m has single child of descendant sort of s', then unwrap
    // otherwise return empty
    failwith("todo")
  | (Grout((s, (l, r))), Tile({sort, prec, zipper: (s', rctx)})) =>
    // may need to unwrap grout or adjust its sort
    // check if m has single child of exactly sort s', then unwrap
    // otherwise check if s leq s' and that neither side of rctx is nullable,
    // in which case then update s to s' and return full
    // otherwise return empty
    failwith("todo")
  | (Tile((s, p, (l, r))), Grout(s')) =>
    // check that s is descendant of s', full if so
    // (relying here on assumption that m is integral)
    failwith("todo")
  | (Tile((s, p, (l, r))), Tile({sort, prec, zipper: (s', rctx)})) =>
    // for each side
    // check if entering from that side into gcell arrives at end tile
    // otherwise, check if entering from pre/postfix grout of sort s' arrives at end tile
    failwith("todo")
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
  type t = Slot.t(ESort.t);

  let consistent = (e: t, g: GSlot.Sorted.t) =>
    switch (e, g) {
    | (Empty, _) => true
    | (Full(_), Empty) => false
    | (Full(e), Full(g)) => ESort.consistent(e, g)
    };
};

let sort: t => Sorted.t = map(EMeld.sort);

let consistent = (e: t, g: GSlot.t) =>
  Sorted.consistent(sort(e), GSlot.sort(g));

let bake: GSlot.t => t = Slot.map(_ => EMeld.mk_hole());
