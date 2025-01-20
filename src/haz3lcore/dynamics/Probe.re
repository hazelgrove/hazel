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

/* A function call at function ap of id */
[@deriving (show({with_path: false}), sexp, yojson)]
type call_frame = Id.t;

/* A call at stack represented by function ap ids */
[@deriving (show({with_path: false}), sexp, yojson)]
type call_stack = list(call_frame);

/* Lexical stack frame */
[@deriving (show({with_path: false}), sexp, yojson)]
type closure_frame = {
  ap_id: Id.t, /* Syntax id of the ap */
  env_id: Id.t /* id of parent closure environment */
};

/* Fixed-depth stack of closures from nested function literals */
[@deriving (show({with_path: false}), sexp, yojson)]
type closure_stack = list(closure_frame);

let empty: t = {refs: []};

let env_stack: list(closure_frame) => list(Id.t) =
  List.map((en: closure_frame) => en.env_id);

let call_stack: list(closure_frame) => list(Id.t) =
  List.map((en: closure_frame) => en.ap_id);

let mk_frame = (~env_id: Id.t, ~ap_id: Id.t): closure_frame => {
  env_id,
  ap_id,
};
