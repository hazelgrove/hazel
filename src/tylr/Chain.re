open Util;

type t = Aba.t(option(kid), Piece.t)
and kid =
  | K(t);

// exception Missing_root_pieces;

module Z = {
  type t = (int, Piece.Z.t);
};

let of_piece = (p: Piece.t) => Aba.mk([None, None], [p]);

let split_uni_kid = (d: Dir.t, c: t): (option(kid), t) =>
  switch (d) {
  | L =>
    let (k, (ps, ks)) = Aba.split_first_a(c);
    (k, ([None, ...ks], ks));
  | R =>
    let ((ks, ps), k) = Aba.split_last_a(c);
    (k, (ks @ [None], ps));
  };

// let tip_l = (c: t) => {
//   let (kid, p) =
//     Aba.first_ab(c) |> OptUtil.get_or_raise(Missing_root_pieces);
//   switch (kid) {
//   | Some(_) => Convex
//   | None => Piece.tip(L, p)
//   };
// };
// let tip_l = (c: t) => {
//   let (p, kid) = Aba.lab_ba(c) |> OptUtil.get_or_raise(Missing_root_pieces);
//   switch (kid) {
//   | Some(_) => Convex
//   | None => Piece.tip(R, p)
//   };
// };
// let tip = (d: Dir.t): (t => Tip.t) =>
//   switch (d) {
//   | L => tip_l
//   | R => tip_r
//   };

// let merge = (l: t, r: t): t =>
// switch (Chain.tip(R, c), Chain.tip(L, hd)) {
// | (Convex, Convex) => raise(Nonmonotonic)
// | (Convex, Concave(_)) =>
// }
