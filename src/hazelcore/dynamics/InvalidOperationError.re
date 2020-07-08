[@deriving sexp]
type t =
  | DivideByZero;

let err_msg = (err: t): string =>
  switch (err) {
  | DivideByZero => "Error: Divide by Zero"
  };
