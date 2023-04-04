open Sexplib.Std;
// open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t('token) =
  | S(Space.t)
  | T('token);
[@deriving (show({with_path: false}), sexp, yojson)]
type s('token) = list(t('token));

let is_space =
  fun
  | T(_) => None
  | S(s) => Some(s);

// let token =
//   fun
//   | G(g) => g.filled
//   | T(t) => t.proto.label
//   | S(s) => Space.to_string(s);

// let id =
//   fun
//   | G(g) => g.id
//   | T(t) => t.id
//   | S(s) => s.id;

// let to_piece =
//   fun
//   | S(s) => Error(s)
//   | G(g) => Ok(Piece.mk(G(g)))
//   | T(t) => Ok(Piece.mk(T(t)));

// let uncons_char = (lx: t): option((t, t)) =>
//   switch (lx) {
//   | S(_) => None
//   | G(g) =>
//     Grout.uncons_char(g) |> Option.map(((hd, tl)) => (G(hd), G(tl)))
//   | T(t) =>
//     Tile.uncons_char(t) |> Option.map(((hd, tl)) => (T(hd), T(tl)))
//   };
// let uncons_char_s = (ls: s): option((t, s)) => {
//   open OptUtil.Syntax;
//   let+ (tl, hd) = ListUtil.split_last_opt(ls);
//   switch (uncons_char(hd)) {
//   | None => (hd, tl)
//   | Some((c, hd)) => (c, [hd, ...tl])
//   };
// };

// let unsnoc_char = (lx: t): option((t, t)) =>
//   switch (lx) {
//   | S(_) => None
//   | G(g) =>
//     Grout.unsnoc_char(g) |> Option.map(((tl, hd)) => (G(tl), G(hd)))
//   | T(t) =>
//     Tile.unsnoc_char(t) |> Option.map(((tl, hd)) => (T(tl), T(hd)))
//   };
// let unsnoc_char_s = (ls: s): option((s, t)) => {
//   open OptUtil.Syntax;
//   let+ (tl, hd) = ListUtil.split_last_opt(ls);
//   switch (unsnoc_char(hd)) {
//   | None => (tl, hd)
//   | Some((hd, c)) => (tl @ [hd], c)
//   };
// };

// let length =
//   fun
//   | T(t) => Tile.length(t)
//   | G(g) => Grout.length(g)
//   | S(s) => Space.length(s);
