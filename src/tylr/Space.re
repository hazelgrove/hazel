open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type shape =
  | Space
  | Newline;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  shape,
};
[@deriving (show({with_path: false}), sexp, yojson)]
type s = list(t);

module Step = {
  // number of spaces to left of cursor
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = int;
};

let empty = [];
let is_empty: s => bool = (==)(empty);
let length: s => int = List.length;

let mk = shape => {
  let id = Id.Gen.next();
  {id, shape};
};

let unzip = (step: Step.t, ss: s): (s, s) => ListUtil.split_n(step, ss);

let is_cursor = (_: t) => failwith("todo split_cursor");

let to_string = s =>
  switch (s.shape) {
  | Space => " "
  | Newline => "\n"
  };
