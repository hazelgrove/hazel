open Sexplib.Std;

module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // top-down
    slots: list(int),
    piece: int,
    offset: int,
  };
};
include Base;

exception Invalid;

module Marked = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('a) = (option(Base.t), 'a);

  let mk = (~path=?, a) => (path, a);
};
