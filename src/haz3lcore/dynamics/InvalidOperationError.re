open Sexplib.Std;
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | DivideByZero
  | NegativeExponent
  | OutOfFuel
  | InvalidProjection
  | LetOperatorsNotDefined(string);

let err_msg = (err: t): string =>
  switch (err) {
  | DivideByZero => "Error: Divide by Zero"
  | NegativeExponent => "Error: Negative Exponent in Integer Exponentiation (Consider using **.)"
  | OutOfFuel => "Error: Out of Fuel"
  | InvalidProjection => "Error: Invalid Projection"
  | LetOperatorsNotDefined(string) =>
    "Error: Let Operators(" ++ string ++ ") Not Defined"
  };
