type shape =
  | Space
  | Newline;
type elem = {
  id: Id.t,
  shape,
};
type t = list(elem);

let empty = [];

let mk_elem = shape => {
  let id = Id.Gen.next();
  {id, shape};
};
