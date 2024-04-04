open Sexplib.Std;

// specifies preferred whitespace padding around a token
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = {
  // whether to pad token with spaces in horizontal layout
  h: (bool, bool),
  // whether to pad token with newlines in vertical layout
  // (generally (true, true) but eg (false, true) for commas)
  v: (bool, bool),
  // whether to indent contents of the following cell
  indent: bool,
};

let mk = (~h_l=true, ~h_r=true, ~v_l=true, ~v_r=true, ~indent=true, ()) => {
  h: (h_l, h_r),
  v: (v_l, v_r),
  indent,
};

let none = {h: (false, false), v: (false, false), indent: false};

// padding for space cells
// let empty = mk(~space=false, ~indent=false, ());

// let root = mk(~space=false, ~indent=false, ());
