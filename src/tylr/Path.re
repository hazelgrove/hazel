open Sexplib.Std;
module Either3 = Util.Either3;

module Kid = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
};
module Lex = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Space(Dir.t)
    | Piece(int);
};
module Col = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  // top-down
  kids: list(Kid.t),
  lex: Lex.t,
  col: Col.t,
};

let mk = (~kids=[], ~col=0, lex) => {kids, lex, col};
// let empty = mk();
let of_space = (side, col) => mk(Space(side), ~col);
let of_piece = (n, col: Col.t) => mk(Piece(n), ~col);

let cons = (kid, path) => {...path, kids: [kid, ...path.kids]};
let cons_s = (kids, path) => List.fold_right(cons, kids, path);

let with_kid = kid =>
  fun
  | {kids: [hd, ...kids], _} as path when hd == kid => Some({...path, kids})
  | _ => None;
let with_piece = n =>
  fun
  | {kids: [], lex: Piece(m), col} when m == n => Some(col)
  | _ => None;
let with_space = side =>
  fun
  | {kids: [], lex: Space(d), col} when d == side => Some(col)
  | _ => None;

let shift_space = (~side, n) =>
  fun
  | {kids: [], lex: Space(d), col} as p when d == side => {
      ...p,
      col: col + n,
    }
  | path => path;

let link = (path: t) =>
  switch (path) {
  | {kids: [hd, ...tl], _} => {...path, kids: [hd + 1, ...tl]}
  | {kids: [], lex: Space(L), _} => {...path, kids: [1]}
  | {kids: [], lex: Space(R), _} => path
  | {kids: [], lex: Piece(n), _} => {...path, lex: Piece(n + 1)}
  };
let knil = (~len, path: t) =>
  switch (path) {
  | {kids: [], lex: Space(L) | Piece(_), _}
  | {kids: [_, ..._], _} => path
  | {kids: [], lex: Space(R), _} => {...path, kids: [len]}
  };

let unlink = (path: t): Either3.t(t, Col.t, t) =>
  switch (path) {
  | {kids: [0, ...kids], _} => L({...path, kids})
  | {kids: [hd, ...tl], _} => R({...path, kids: [hd - 1, ...tl]})
  | {kids: [], lex: Space(L), _} => L(path)
  | {kids: [], lex: Space(R), _} => R(path)
  | {kids: [], lex: Piece(0), col} => M(col)
  | {kids: [], lex: Piece(n), _} => R({...path, lex: Piece(n - 1)})
  };
let unknil = (~len, path: t): Either3.t(t, int, t) =>
  switch (path) {
  | {kids: [n, ...kids], _} when n == len => R({...path, kids})
  | {kids: [_, ..._], _} => L(path)
  | {kids: [], lex: Space(R), _} => R(path)
  | {kids: [], lex: Space(L), _} => L(path)
  | {kids: [], lex: Piece(n), col} => n == len - 1 ? M(col) : L(path)
  };
