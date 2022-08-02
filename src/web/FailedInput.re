open Core;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type reason =
  | Unrecognized
  | Failure(Perform.Action.Failure.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  reason,
  prior_attempts: int,
};

let mk = (~prior_attempts=0, reason) => {reason, prior_attempts};

let replace_or_increment_attempts = (reason, failed_input: t) =>
  reason == failed_input.reason
    ? {...failed_input, prior_attempts: failed_input.prior_attempts + 1}
    : mk(reason);
