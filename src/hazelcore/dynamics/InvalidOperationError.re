[@deriving sexp]
type t =
  | DivideByZero
  | IndexOutBound
  | IllegalEscape
  | StrNotTerminate;

let err_msg = (err: t): string =>
  switch (err) {
  | DivideByZero => "Error: Divide by Zero"
  | IndexOutBound => "Error: Index out of bound"
  | IllegalEscape => "Error: illegal backslash escape in string"
  | StrNotTerminate => "Error: String literal not terminated"
  };
