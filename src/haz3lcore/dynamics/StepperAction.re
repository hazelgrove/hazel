[@deriving (show, sexp, yojson)]
type t =
  | Pause
  | Step
  | Eval;
