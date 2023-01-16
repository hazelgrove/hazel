open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Aba.t(option(kid), Piece.t)
[@deriving (show({with_path: false}), sexp, yojson)]
and kid =
  | K(t);

exception Missing_root;

let empty = Aba.singleton(None);

module Padded = {
  type c = t;
  // chain with padding (ie single-chain segment)
  type t = (c, (Space.t, Space.t));

  let mk = (~l=Space.empty, ~r=Space.empty, c) => (c, (l, r));

  let pad = (~l=Space.empty, ~r=Space.empty, (c, (l', r')): t) => (
    c,
    (l @ l', r' @ r),
  );
};

let sort = _ => failwith("todo sort");
let prec = _ => failwith("todo prec");

let uncons = (_: t) => failwith("todo uncons");

let cmp_mold = (_: t, _: Mold.t): option(Cmp.t) =>
  failwith("todo cmp_mold");
let cmp = (_: t, _: t): Cmp.t => failwith("todo cmp");

let finish_l = (~kid as _=?, _) => failwith("todo finish_l");
let finish_r = (_, ~kid as _=?, ()) => failwith("todo finish_r");

// accepts empty chains if expected arg provided
[@warning "-27"]
let finish = (~expected: option(Sort.t)=?, _) => failwith("todo finish");

let mk_kid = c => K(finish(c));

let match_ = (_, ~kid as _=?, _) => failwith("todo match_");

let cmp_merge = (l: t, ~kid=?, r: t): Cmp.Result.t(t, Padded.t, t, Padded.t) =>
  switch (cmp(l, r)) {
  | In =>
    assert(kid == None);
    let m = Mold.mk_infix((sort(l), prec(l)), (sort(r), prec(r)));
    let g = Grout.mk(m);
    Cmp.Result.In(
      Aba.mk(
        [Some(mk_kid(l)), Some(mk_kid(r))],
        // todo: add padding
        [Piece.mk(G(g))],
      ),
    );
  | Lt => Lt(finish_l(~kid?, r))
  | Eq => Eq(match_(l, ~kid?, r))
  | Gt => Gt(finish_r(l, ~kid?, ()))
  };

[@warning "-27"]
let mold = (c: t, ~kid=?, t: Token.t) => failwith("todo Chain.mold");

let sort = _ => failwith("todo Chain.sort");
let expected_sort = (_: Dir.t, _) => failwith("todo Chain.expected_sort");

let of_piece = (p: Piece.t) => Aba.mk([None, None], [p]);
let of_grout = (g: Grout.t) => of_piece(Piece.mk(G(g)));
let of_tile = (t: Tile.t) => of_piece(Piece.mk(T(t)));

[@warning "-27"]
let pop_lexeme = (~from: Dir.t, _) => failwith("todo pop_lexeme");

// todo: probably want to replace with lexeme
[@warning "-27"]
let pop_token = (~from: Dir.t, _) => failwith("todo pop_token");

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
