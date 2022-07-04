[@deriving sexp]
type t =
  | OutOfFuel
  | DivideByZero;

let err_msg = (err: t): string =>
  switch (err) {
  | OutOfFuel => "Error: Out of Fuel"
  | DivideByZero => "Error: Divide by Zero"
  };
