open Sexplib.Std;

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

let empty = [];
let is_empty: t => bool = (==)(empty);

let mk_elem = shape => {
  let id = Id.Gen.next();
  {id, shape};
};

let is_cursor = (_: elem) => failwith("todo split_cursor");

let to_string = s =>
  switch (s.shape) {
  | Space => " "
  | Newline => "\n"
  };
