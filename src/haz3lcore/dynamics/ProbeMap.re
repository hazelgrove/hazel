[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  id: Id.t,
  env: Environment.t,
  res: EvaluatorResult.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Id.Map.t(entry);
let empty: t = Id.Map.empty;

let add = Id.Map.add;
