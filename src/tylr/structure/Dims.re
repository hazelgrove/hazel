open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  height: int, // number of newlines
  width: int // number of characters in last line
};

let mk = (~height=0, width) => {height, width};
let zero = mk(0);

// associative, not commutative
let add = (l: t, r: t) => {
  height: l.height + r.height,
  width: (r.height == 0 ? l.width : 0) + r.width,
};
let sum = List.fold_left(add, zero);
