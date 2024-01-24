module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cell: Cell.t,
    wald: Wald.t,
  };
};
include Base;

module L = {
  // L2R: wald cell
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t;
};
module R = {
  // L2R: cell wald
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Base.t;
};
