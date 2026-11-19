[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type action =
  | Step
  | Eval;

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type count =
  | One
  | All;

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type t = (action, count);

let string_of_t = action => {
  switch (action) {
  | (Step, One) => "stop"
  | (Step, All) => "step"
  | (Eval, One) => "hide"
  | (Eval, All) => "eval"
  };
};

/* Derived from string_of_t so the two can never fall out of sync. */
let t_of_string = (s: string): option(t) =>
  List.find_opt(a => String.equal(string_of_t(a), s), all);
