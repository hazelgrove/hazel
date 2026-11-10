/* Left or right: a side of something, or which way to move. */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Left
  | Right;
let toggle: t => t;
