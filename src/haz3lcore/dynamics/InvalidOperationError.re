[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | DivideByZero
  | OutOfFuel;

let err_msg = (err: t): string =>
  switch (err) {
  | DivideByZero => "Error: Divide by Zero"
  | OutOfFuel => "Error: Out of Fuel"
  };
