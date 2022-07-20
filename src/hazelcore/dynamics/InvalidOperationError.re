[@deriving sexp]
type t =
  | OutOfGas
  | DivideByZero;

let err_msg = (err: t): string =>
  switch (err) {
  | OutOfGas => "Error: Recursion Limit Exceeded"
  | DivideByZero => "Error: Divide by Zero"
  };
