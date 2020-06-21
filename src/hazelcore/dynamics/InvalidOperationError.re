[@deriving sexp]
type t =
  | DivideByZero
  | StartOutBound
  | EndOutBound
  | StartEndOutBound
  | GreaterStart
  | IllegalEscape;

let err_msg = (err: t): string =>
  switch (err) {
  | DivideByZero => "Error: Divide by Zero"
  | StartOutBound => "Error: Starting subscript out of bound"
  | EndOutBound => "Error: Ending subscript out of bound"
  | StartEndOutBound => "Error: Starting and ending subscript out of bound"
  | GreaterStart => "Error: Starting subscript greater than ending subscript"
  | IllegalEscape => "Error: illegal backslash escape in string"
  };
