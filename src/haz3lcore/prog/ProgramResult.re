[@deriving (show({with_path: false}), sexp, yojson)]
type t = (EvaluatorResult.t, EvaluatorState.t);

let get_dhexp = ((r, _): t) => EvaluatorResult.unbox(r);
let get_state = ((_, es): t) => es;
