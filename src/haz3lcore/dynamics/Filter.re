[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  pat: TermBase.UExp.t,
  act: FilterAction.t,
};

let mk = (pat: TermBase.UExp.t, act: FilterAction.t): t => {pat, act};

let map = (f: TermBase.UExp.t => TermBase.UExp.t, filter: t): t => {
  ...filter,
  pat: f(filter.pat),
};
