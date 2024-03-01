open Sexplib.Std;
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | InvalidOfString
  | IndexOutOfBounds
  | DivideByZero
  | NegativeExponent
  | OutOfFuel
  | LetOperatorsNotDefined(string);

let err_msg = (err: t): string =>
  switch (err) {
  | InvalidOfString => "Error: Invalid String Conversion"
  | IndexOutOfBounds => "Error: Index Out of Bounds"
  | DivideByZero => "Error: Divide by Zero"
  | NegativeExponent => "Error: Negative Exponent in Integer Exponentiation (Consider using **.)"
  | OutOfFuel => "Error: Out of Fuel"
  | LetOperatorsNotDefined(string) =>
    "Error: Let Operators(" ++ string ++ ") Not Defined"
  };
