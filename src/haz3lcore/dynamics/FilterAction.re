[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Keep
  | Step
  | Eval;

let decr =
  fun
  | Keep => Keep
  | Step => Keep
  | Eval => Eval;

let t_of_uexp = (act: TermBase.UExp.filter_action): t => {
  switch (act) {
  | Step => Step
  | Eval => Eval
  };
};
