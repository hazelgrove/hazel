open Util;

/* A syntax probe is inserted into syntax to capture
 * information during evaluation. The tag type below,
 * for the probe case, is used to collect binding ids
 * which are used to faciltate capturing the values
 * of certain variables in the environment. This captured
 * information is, for a given closure, encoded in
 * the `frame` type. */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  refs: Binding.s,
  stem: Binding.stem,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type tag =
  | Paren
  | Probe(t);

/* Information about the evaluation of an ap */
[@deriving (show({with_path: false}), sexp, yojson)]
type frame = {
  ap_id: Id.t, /* Syntax ID of the ap */
  env_id: Id.t /* ID of ClosureEnv created by ap  */
};

/* List of applications prior to some evaluation */
[@deriving (show({with_path: false}), sexp, yojson)]
type stack = list(frame);

let empty: t = {refs: [], stem: []};

let env_stack: list(frame) => list(Id.t) =
  List.map((en: frame) => en.env_id);

let mk_frame = (~env_id: Id.t, ~ap_id: Id.t): frame => {env_id, ap_id};
