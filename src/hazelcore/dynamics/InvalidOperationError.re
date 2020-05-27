[@deriving sexp]
type t =
  | DivideByZero
  | IndexOutBound;

let err_msg = (err: t): string =>
  switch (err) {
  | DivideByZero => "Error: Divide by Zero"
  | IndexOutBound => "Error: Index out of bound"
  };
