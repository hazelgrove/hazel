[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | Step
  | Eval;

[@deriving (show({with_path: false}), sexp, yojson)]
type count =
  | One
  | All;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = (action, count);
