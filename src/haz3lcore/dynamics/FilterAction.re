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

let t_of_string = s => {
  switch (s) {
  | "stop" => Some((Step, One))
  | "step" => Some((Step, All))
  | "hide" => Some((Eval, One))
  | "eval" => Some((Eval, All))
  | _ => None
  };
};

let string_of_t = v => {
  switch (v) {
  | (Step, One) => "stop"
  | (Step, All) => "step"
  | (Eval, One) => "hide"
  | (Eval, All) => "eval"
  };
};
