[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | InvalidOfString
  | IndexOutOfBounds
  | DivideByZero
  | NegativeExponent
  | NegativeNat
  | IntegerTooBig
  /* an SInt result outside 32 bits (Atom.sint_fits) */
  | IntegerOverflow
  | Incomparable;

let err_msg = (err: t): string =>
  switch (err) {
  | InvalidOfString => "Error: Invalid String Conversion"
  | IndexOutOfBounds => "Error: Index Out of Bounds"
  | DivideByZero => "Error: Divide by Zero"
  | NegativeExponent => "Error: Negative Exponent in Integer Exponentiation (Consider using **.)"
  | NegativeNat => "Error: Cannot convert negative number to Nat"
  | IntegerTooBig => "Error: Integer too big"
  | IntegerOverflow => "Error: Integer overflow (an SInt has 32 bits)"
  | Incomparable => "Error: Incomparable Types"
  };
