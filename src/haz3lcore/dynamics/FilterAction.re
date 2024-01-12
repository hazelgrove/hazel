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
  | (Eval, One) => "concel"
  | (Eval, All) => "eval"
  };
};
