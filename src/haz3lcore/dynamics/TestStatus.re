[@deriving (show({with_path: false}), sexp, yojson)]
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
    | (Indet, Fail) => Fail
    | (Indet, _) => Indet
    | (Pass, x) => x
    };

let join_all: list(t) => t = List.fold_left(join, Pass);
