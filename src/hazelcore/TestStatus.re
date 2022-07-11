[@deriving sexp]
type t =
  | Pass
  | Fail
  | Indet;

let to_string =
  fun
  | Pass => "Pass"
  | Fail => "Fail"
  | Indet => "Indet";

let join = (s, s') =>
  switch (s, s') {
  | (Fail, _) => Fail
  | (Indet, Fail) => Fail
  | (Indet, _) => Indet
  | (Pass, x) => x
  };

let join_all = List.fold_left(join, Pass);
