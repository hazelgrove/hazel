open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type paint =
  | Type(Typ.head);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  id: Id.t,
  level: int,
  paint,
};

let paint_name: paint => string =
  fun
  | Type(head) => "type-block-" ++ Typ.head_name(head);
