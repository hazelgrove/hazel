module HoleReason = {
  [@deriving sexp]
  type t =
    | Free
    | TypeInconsistent;
};
