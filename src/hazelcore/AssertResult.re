[@deriving sexp]
type t =
  | Pass
  | Fail
  | Indet
  | Comp /*only used for chec*/;

// TODO(andrew): this is wrong
let join: (t, t) => t =
  (a, b) =>
    switch (a, b) {
    | (Pass, x) => x
    | (Fail, _) => Fail
    | (Indet, _) => Indet
    | (Comp, _) => Comp
    };

let join_all: list(t) => t = xs => List.fold_left(join, Pass, xs);
