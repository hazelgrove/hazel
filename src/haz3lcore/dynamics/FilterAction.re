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

let string_of_t = v => {
  switch (v) {
  | (Step, One) => "pause"
  | (Step, All) => "debug"
  | (Eval, One) => "hide"
  | (Eval, All) => "eval"
  };
};

let of_menhir_ast = (a: Hazel_menhir.AST.filter_action): t => {
  switch (a) {
  | Eval => (Eval, All)
  | Pause => (Step, One)
  | Debug => (Step, All)
  | Hide => (Eval, One)
  };
};
