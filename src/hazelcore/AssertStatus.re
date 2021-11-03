[@deriving sexp]
type t =
  | Pass
  | Fail
  | Indet;

let to_string: t => string =
  fun
  | Pass => "Pass"
  | Fail => "Fail"
  | Indet => "Indet";

let join: (t, t) => t =
  (a, b) =>
    switch (a, b) {
    | (Fail, _) => Fail
    | (_, x) => x
    };

let join_all: list(t) => t = xs => List.fold_left(join, Pass, xs);
