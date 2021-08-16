/**
 * Move keys handled by `CursorMap`
 */
[@deriving (sexp, show)]
type t =
  | ArrowLeft
  | ArrowRight
  | ArrowUp
  | ArrowDown
  | Home
  | End;
