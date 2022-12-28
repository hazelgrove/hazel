open Util;

type t = Aba.t(option(kid), Piece.t)
and kid =
  | K(t);

exception Missing_root_pieces;

let tip_l = (c: t) => {
  let (kid, p) =
    Aba.first_ab(c) |> OptUtil.get_or_raise(Missing_root_pieces);
  switch (kid) {
  | Some(_) => Convex
  | None => Piece.tip(L, p)
  };
};
let tip_l = (c: t) => {
  let (p, kid) = Aba.lab_ba(c) |> OptUtil.get_or_raise(Missing_root_pieces);
  switch (kid) {
  | Some(_) => Convex
  | None => Piece.tip(R, p)
  };
};
let tip = (d: Dir.t): (t => Tip.t) =>
  switch (d) {
  | L => tip_l
  | R => tip_r
  };

// let merge = (l: t, r: t): t =>
// switch (Chain.tip(R, c), Chain.tip(L, hd)) {
// | (Convex, Convex) => raise(Nonmonotonic)
// | (Convex, Concave(_)) =>
// }
