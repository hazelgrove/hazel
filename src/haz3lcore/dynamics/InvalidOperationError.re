[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | DivideByZero
  | NegativeExponent
  | OutOfFuel
  | InvalidProjection;

let err_msg = (err: t): string =>
  switch (err) {
  | DivideByZero => "Error: Divide by Zero"
  | NegativeExponent => "Error: Negative Exponent in Integer Exponentiation (Consider using **.)"
  | OutOfFuel => "Error: Recursion Limit Exceeded"
  | InvalidProjection => "Error: Invalid Projection"
  };
