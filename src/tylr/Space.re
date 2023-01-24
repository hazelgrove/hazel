open Sexplib.Std;

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

let empty = [];
let is_empty: s => bool = (==)(empty);

let mk = shape => {
  let id = Id.Gen.next();
  {id, shape};
};

let is_cursor = (_: t) => failwith("todo split_cursor");

let to_string = s =>
  switch (s.shape) {
  | Space => " "
  | Newline => "\n"
  };
