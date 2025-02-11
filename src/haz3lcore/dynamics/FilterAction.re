[@deriving (show({with_path: false}), sexp, yojson, eq)]
type action =
  | Step
  | Eval;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type count =
  | One
  | All;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = (action, count);

let string_of_t = v => {
  switch (v) {
  | (Step, One) => "pause"
  | (Step, All) => "debug"
  | (Eval, One) => "hide"
  | (Eval, All) => "eval"
  };
};
