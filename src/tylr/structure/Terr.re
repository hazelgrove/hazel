module Base = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cell: Cell.t,
    wald: Wald.t,
  };
};
include Base;

module L = {
  type t = Base.t;
};

module R = {
  type t = Base.t;
};
