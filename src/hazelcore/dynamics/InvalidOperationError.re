[@deriving sexp]
type t =
  | DivideByZero
  | IndexOutBound
  | StrNotConvToInt
  | StrNotConvToFloat
  | StrNotConvToBool
  | IntOutBound
  | StrNotTerminate
  | StartOutBound
  | EndOutBound
  | StartEndOutBound
  | IllegalEscape;

let err_msg = (err: t): string =>
  switch (err) {
  | DivideByZero => "Error: Divide by Zero"
  | StartOutBound => "Error: Starting subscript out of bound"
  | EndOutBound => "Error: Ending subscript out of bound"
  | StartEndOutBound => "Error: Starting and ending subscript out of bound"
  | IllegalEscape => "Error: illegal backslash escape in string"
  | IndexOutBound => "Error: Index out of bound"
  | StrNotConvToInt => "Error: String cannot be converted to Int"
  | StrNotConvToFloat => "Error: String cannot be converted to Float"
  | StrNotConvToBool => "Error: String cannot be converted to Bool"
  | IntOutBound => "Error: Integer literal exceeds the range of representable integers of type int"
  | StrNotTerminate => "Error: String literal not terminated"
  };
