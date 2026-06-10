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

let string_of_t = action => {
  switch (action) {
  | (Step, One) => "stop"
  | (Step, All) => "step"
  | (Eval, One) => "hide"
  | (Eval, All) => "eval"
  };
};

let t_of_string = s => {
  switch (s) {
  | "stop" => Some((Step, One))
  | "step" => Some((Step, All))
  | "hide" => Some((Eval, One))
  | "eval" => Some((Eval, All))
  | _ => None
  };
};
