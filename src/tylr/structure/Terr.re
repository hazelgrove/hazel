module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = {
    cell: Cell.t,
    wald: Wald.t,
  };
};

module L = {
  include Base;
  // include L;
};

module R = {
  include Base;
  // include R;
};
