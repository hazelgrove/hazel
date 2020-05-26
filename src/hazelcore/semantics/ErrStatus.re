module HoleReason = {
  /* Variable: `reason` */
  [@deriving sexp]
  type t =
    | TypeInconsistent
    | WrongLength;
};

module RunTime = {
  [@deriving sexp]
  type t =
    | DivideByZero
    | IndexOutBound;

  let err_msg = (err: t): string =>
    switch (err) {
    | DivideByZero => "Error: Divide by Zero"
    | IndexOutBound => "Error: Index out of bound"
    };
};
/* Variable: `err` */
[@deriving sexp]
type t =
  | NotInHole
  | InHole(HoleReason.t, MetaVar.t);
