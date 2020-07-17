/**
 * Move keys handled by `CursorMap`
 */
[@deriving sexp]
type t =
  | ArrowLeft
  | ArrowRight
  | ArrowUp
  | ArrowDown
  | Home
  | End;

let of_key: string => option(t);
