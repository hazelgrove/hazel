open Sexplib.Std;
module Either3 = Util.Either3;

[@deriving (show({with_path: false}), sexp, yojson)]
type here =
  | Space(Dir.t, Space.Path.t)
  | Piece(int, Piece.Path.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  // top-down
  kids: list(int),
  here,
};

let mk = (~kids=[], ~here=Space(L, 0), ()) => {kids, here};
let empty = mk();
let of_space = (side, col) => mk(~here=Space(side, col), ());
let of_piece = (index, col: Piece.Path.t) =>
  mk(~here=Piece(index, col), ());

let cons = (kid, path) => {...path, kids: [kid, ...path.kids]};
let cons_s = (kids, path) => List.fold_right(cons, kids, path);

let with_kid = kid =>
  fun
  | {kids: [hd, ...kids], _} as path when hd == kid => Some({...path, kids})
  | _ => None;
let with_piece = index =>
  fun
  | {kids: [], here: Piece(i, col)} when i == index => Some(col)
  | _ => None;
let with_space = side =>
  fun
  | {kids: [], here: Space(d, col)} when d == side => Some(col)
  | _ => None;

let shift_space = (~side, n) =>
  fun
  | {kids: [], here: Space(d, col)} when d == side => {
      kids: [],
      here: Space(side, col + n),
    }
  | path => path;

let link = (path: t) =>
  switch (path) {
  | {kids: [hd, ...tl], _} => {...path, kids: [hd + 1, ...tl]}
  | {kids: [], here: Space(L, _)} => {...path, kids: [1]}
  | {kids: [], here: Space(R, _)} => path
  | {kids: [], here: Piece(index, col)} => {
      ...path,
      here: Piece(index + 1, col),
    }
  };
let knil = (~len, path: t) =>
  switch (path) {
  | {kids: [], here: Space(L, _) | Piece(_), _}
  | {kids: [_, ..._], _} => path
  | {kids: [], here: Space(R, _), _} => {...path, kids: [len]}
  };

let unlink = (path: t): Either3.t(t, int, t) =>
  switch (path) {
  | {kids: [0, ...kids], _} => L({...path, kids})
  | {kids: [hd, ...tl], _} => R({...path, kids: [hd - 1, ...tl]})
  | {kids: [], here: Space(L, _)} => L(path)
  | {kids: [], here: Space(R, _)} => R(path)
  | {kids: [], here: Piece(0, col)} => M(col)
  | {kids: [], here: Piece(index, col)} =>
    R({...path, here: Piece(index - 1, col)})
  };
let unknil = (~len, path: t): Either3.t(t, int, t) =>
  switch (path) {
  | {kids: [n, ...kids], _} when n == len => R({...path, kids})
  | {kids: [_, ..._], _} => L(path)
  | {kids: [], here: Space(R, _)} => R(path)
  | {kids: [], here: Space(L, _)} => L(path)
  | {kids: [], here: Piece(index, col)} =>
    index == len - 1 ? M(col) : L(path)
  };
