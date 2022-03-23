/* Fill-and-resume information for evaluation. */
[@deriving sexp]
type t =
  | NonFill
  | Fill(MetaVar.t, DHExp.t);

let empty: t = NonFill;
