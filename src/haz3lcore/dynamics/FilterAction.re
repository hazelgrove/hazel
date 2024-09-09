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
  | "pause" => Some((Step, One))
  | "debug" => Some((Step, All))
  | "hide" => Some((Eval, One))
  | "eval" => Some((Eval, All))
  | _ => None
  };
};

let string_of_t = v => {
  switch (v) {
  | (Step, One) => "pause"
  | (Step, All) => "debug"
  | (Eval, One) => "hide"
  | (Eval, All) => "eval"
  };
};
