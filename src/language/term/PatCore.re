open Util;
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type pat_term('pat, 'typ, 'any) =
  | Invalid(string)
  | EmptyHole
  | MultiHole(list('any))
  | Wild
  | ExplicitNonlabel
  | Atom(Atom.t)
  | ListLit(list('pat))
  | Constructor(string, option(option('typ))) // see comment on constructor expressions
  | Cons('pat, 'pat)
  | Var(Var.t)
  | Tuple(list('pat))
  | Label(string)
  | TupLabel('pat, 'pat)
  | Parens('pat)
  | Probe('pat, Probe.t)
  | Ap('pat, 'pat)
  | Asc('pat, 'typ);
