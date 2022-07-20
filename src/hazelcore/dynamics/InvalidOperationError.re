[@deriving sexp]
type t =
  | OutOfFuel
  | DivideByZero;

let err_msg = (err: t): string =>
  switch (err) {
  | OutOfFuel => "Error: Recursion Limit Exceeded"
  | DivideByZero => "Error: Divide by Zero"
  };
