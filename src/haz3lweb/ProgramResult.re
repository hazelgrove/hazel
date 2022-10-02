open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type ok = (EvaluatorResult.t, EvaluatorState.t, HoleInstanceInfo.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Ok(ok)
  | Error(EvaluatorError.t);

let get_dhexp = (result: t): option(DHExp.t) =>
  switch (result) {
  | Ok((r, _, _)) => Some(EvaluatorResult.unbox(r))
  | Error(_) => None
  };

let get_state = (result: t): option(EvaluatorState.t) =>
  switch (result) {
  | Ok((_, es, _)) => Some(es)
  | Error(_) => None
  };

let get_hii = (result: t): option(HoleInstanceInfo.t) =>
  switch (result) {
  | Ok((_, _, hii)) => Some(hii)
  | Error(_) => None
  };

let fast_equal = (result1: t, result2: t): bool =>
  switch (result1, result2) {
  | (Ok((r1, _, hii1)), Ok((r2, _, hii2))) =>
    HoleInstanceInfo.fast_equal(hii1, hii2)
    && EvaluatorResult.fast_equal(r1, r2)
  | _ => false
  };
