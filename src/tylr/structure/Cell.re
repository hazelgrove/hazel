open Util;

include Meld.Cell;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Meld.Cell.t(Meld.t);

let mk = (~marks=Path.Marks.empty, ~meld=?, ()): t => {
  let _ = failwith("todo: lift marks from meld");
  {marks, meld};
};
let empty = mk();
let is_empty = (==)(empty);

let has_space = (cell: t) =>
  switch (cell.meld) {
  | Some(M(_, W(([tok], [])), _)) when Token.is_space(tok) => true
  | _ => false
  };

let add_marks = (marks, cell) => {
  ...cell,
  marks: Path.Marks.union(marks, cell.marks),
};
let clear_marks = cell => {...cell, marks: Path.Marks.empty};

let get = ({marks, meld}: t) => {
  open OptUtil.Syntax;
  let+ Meld.M(l, W((toks, cells)), r) = meld;
  let n = List.length(toks);
  let l = l |> add_marks(Path.Marks.uncons(0, marks));
  let cells =
    cells
    |> List.mapi((i, cell) =>
         cell |> add_marks(Path.Marks.uncons(i + 1, marks))
       );
  let r = r |> add_marks(Path.Marks.uncons(n, marks));
  Meld.M(l, W((toks, cells)), r);
};

let put = (m: Meld.t) => {
  let M(l, W((toks, cells)), r) = m;
  let n = List.length(toks);
  let marks =
    Path.Marks.(
      union_all(
        [
          cons(0, l.marks),
          ...cells |> List.mapi((i, cell) => cons(i + 1, cell.marks)),
        ]
        @ [cons(n, r.marks)],
      )
    );
  mk(~marks, ~meld=Meld.map_cells(clear_marks, m), ());
};

let rec pad = (~side as d: Dir.t, ~pad as p: t, cell: t) =>
  switch (get(cell)) {
  | None => add_marks(cell.marks, p)
  | Some(M(l, w, r)) =>
    let (c_d, c_b) = Dir.order(d, (l, r));
    let c_d = pad(~side=d, ~pad=p, c_d);
    let (l, r) = Dir.order(d, (c_d, c_b));
    put(M(l, w, r));
  };

let face = (~side: Dir.t, cell: t) =>
  cell.meld
  |> Option.map(Meld.face(~side))
  |> Option.value(~default=Molded.Label.space);

// let face = (~side: Dir.t, cell: t) =>
//   switch (cell.meld) {
//   | None => Molded.Label.
//   }

// returns a cell carrying the input meld repaired for sort consistency,
// provided that such repair is possible. otherwise return empty cell.
// let try_fill = (m: EMeld.Marked.t, s: ESort.t) =>
//   switch (EMeld.sorted(snd(m)), s) {
//   | (Space, _) => Full(m)
//   | (_, Space) => Empty
//   | (Grout((s, (l, r))), Grout(s')) =>
//     // check to see if it's possible to unwrap grout
//     // if m has single child of descendant sort of s', then unwrap
//     // otherwise return empty
//     failwith("todo")
//   | (Grout((s, (l, r))), Tile({sort, prec, zipper: (s', rctx)})) =>
//     // may need to unwrap grout or adjust its sort
//     // check if m has single child of exactly sort s', then unwrap
//     // otherwise check if s leq s' and that neither side of rctx is nullable,
//     // in which case then update s to s' and return full
//     // otherwise return empty
//     failwith("todo")
//   | (Tile((s, p, (l, r))), Grout(s')) =>
//     // check that s is descendant of s', full if so
//     // (relying here on assumption that m is integral)
//     failwith("todo")
//   | (Tile((s, p, (l, r))), Tile({sort, prec, zipper: (s', rctx)})) =>
//     // for each side
//     // check if entering from that side into gcell arrives at end tile
//     // otherwise, check if entering from pre/postfix grout of sort s' arrives at end tile
//     failwith("todo")
//   };

// let clear =
//   fun
//   | Empty => []
//   | Full((_, M(l, w, r))) => clear(l) @ clear_wald(w) @ clear(r)
// and clear_wald = w |> Chain.to_list(Piece.clear, clear) |> List.concat;

// let has_no_tiles =
//   fun
//   | Empty => Some("")
//   | Full(m) => EMeld.has_no_tiles(m);

// module Sorted = {
//   type t = Slot.t(ESort.t);

//   let consistent = (e: t, g: GSlot.Sorted.t) =>
//     switch (e, g) {
//     | (Empty, _) => true
//     | (Full(_), Empty) => false
//     | (Full(e), Full(g)) => ESort.consistent(e, g)
//     };
// };

// let sort: t => Sorted.t = map(EMeld.sort);
