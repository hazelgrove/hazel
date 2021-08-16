[@deriving (sexp, show)]
type t =
  | ArrowLeft
  | ArrowRight
  | ArrowUp
  | ArrowDown
  | Home
  | End;
