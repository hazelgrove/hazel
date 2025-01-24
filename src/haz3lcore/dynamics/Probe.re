open Util;

/* A syntax probe is inserted into syntax to capture
 * information during evaluation. The tag type below,
 * for the probe case, is used to collect binding ids
 * which are used to faciltate capturing the values
 * of certain variables in the environment. This captured
 * information is, for a given closure, encoded in
 * the `frame` type. */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {refs: Binding.s};

[@deriving (show({with_path: false}), sexp, yojson)]
type tag =
  | Paren
  | Probe(t);

/* A call at stack represented by function ap ids */
[@deriving (show({with_path: false}), sexp, yojson)]
type call_stack = list(Id.t);

let empty: t = {refs: []};
