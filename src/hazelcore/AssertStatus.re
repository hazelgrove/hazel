[@deriving sexp]
type t =
  | Pass
  | Fail
  | Indet
  | Comp /*only used for chec*/;

let to_string: t => string =
  fun
  | Pass => "Pass"
  | Fail => "Fail"
  | Indet => "Indet"
  | Comp => "Comp";

// TODO(andrew): this is wrong
let join: (t, t) => t =
  (a, b) =>
    switch (a, b) {
    | (Pass, x) => x
    | (Fail, _) => Fail
    | (Indet, x) => x
    | (Comp, _) => Comp
    };

let join_all: list(t) => t = xs => List.fold_left(join, Pass, xs);

let assert_ty: HTyp.t = Arrow(Bool, Prod([]));

let name: string = "assert";
let add_assert_eq = (n, assert_eqs) =>
  switch (List.assoc_opt(n, assert_eqs)) {
  | None => []
  | Some(curr_eqs) => curr_eqs
  };
