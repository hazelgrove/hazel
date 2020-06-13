[@deriving sexp]
type t =
  | DivideByZero
  | IndexOutBound
  | IllegalEscape
  | StrNotConvToInt
  | StrNotConvToFloat
  | StrNotConvToBool
  | IntOutBound
  | StrNotTerminate;

let err_msg = (err: t): string =>
  switch (err) {
  | DivideByZero => "Error: Divide by Zero"
  | IndexOutBound => "Error: Index out of bound"
  | IllegalEscape => "Error: illegal backslash escape in string"
  | StrNotConvToInt => "Error: String cannot be converted to Int"
  | StrNotConvToFloat => "Error: String cannot be converted to Float"
  | StrNotConvToBool => "Error: String cannot be converted to Bool"
  | IntOutBound => "Error: Integer literal exceeds the range of representable integers of type int"
  | StrNotTerminate => "Error: String literal not terminated"
  };
