open Sexplib.Std;

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
