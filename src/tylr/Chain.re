open Util;

type t = Aba.t(option(kid), Piece.t)
and kid =
  | K(t);

// exception Missing_root_pieces;

let cmp_mold = (_: t, _: Mold.t): option(Cmp.t) =>
  failwith("todo cmp_mold");
let cmp = (_: t, _: t): option(Cmp.t) => failwith("todo cmp");

let finish_l = (~kid as _=?, _) => failwith("todo finish_l");
let finish_r = (_, ~kid as _=?, ()) => failwith("todo finish_r");
let match_ = (_, ~kid as _=?, _) => failwith("todo match_");

let cmp_merge = (l: t, ~kid=?, r: t): option(Cmp.Result.t(t, t, t)) => {
  open OptUtil.Syntax;
  let+ cmp = cmp(l, r);
  switch (cmp) {
  | Lt => Cmp.Result.Lt(finish_l(~kid?, r))
  | Gt => Gt(finish_r(l, ~kid?, ()))
  | Eq => Eq(match_(l, ~kid?, r))
  };
};

[@warning "-27"]
let mold = (c: t, ~kid=?, t: Token.t) => failwith("todo Chain.mold");

let sort = _ => failwith("todo Chain.sort");
let expected_sort = (_: Dir.t, _) => failwith("todo Chain.expected_sort");

let of_piece = (p: Piece.t) => Aba.mk([None, None], [p]);
let of_grout = (g: Grout.t) => of_piece(G(g));
let of_tile = (t: Tile.t) => of_piece(T(t));

[@warning "-27"]
let pop_lexeme = (~from: Dir.t, _) => failwith("todo pop_lexeme");

let split_uni_kid = (d: Dir.t, c: t): (option(kid), t) =>
  switch (d) {
  | L =>
    let (k, (ps, ks)) = Aba.split_first_a(c);
    (k, ([None, ...ks], ps));
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
