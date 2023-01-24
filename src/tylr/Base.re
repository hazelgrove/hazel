open Sexplib.Std;
open Util;

module Tile = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    id: Id.t,
    mold: Mold.t,
    token: Token.t,
  };
};

module Space = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type shape =
    | Space
    | Newline;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type elem = {
    id: Id.t,
    shape,
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(elem);
};

module Lexeme = {
  type t =
    | S(Space.elem)
    | G(Grout.t)
    | T(Tile.t);
};

module Piece = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type shape =
    | T(Tile.t)
    | G(Grout.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    shape,
    space: (Space.t, Space.t),
  };
};

module Meld = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Aba.t(option(kid), Piece.t)
  [@deriving (show({with_path: false}), sexp, yojson)]
  and kid =
    | K(t);
};

module Segment = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Aba.t(Space.t, Meld.t);
};
