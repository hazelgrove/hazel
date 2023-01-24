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

let split_cursor = (_: t) => failwith("todo split_cursor");
