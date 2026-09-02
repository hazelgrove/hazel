open Util;

// This module defines the call stack representation used to record probe samples.

/* A single frame in the call stack: app_id + optional function_name.
 * function_name is extracted at evaluation time from the closure/function.
 * fn_def_id is the definition-site ID of the function, extracted from the
 * Closure at evaluation time. Enables jump-to-definition even when app_id
 * comes from built-in internal code (not in user's info_map).
 * The name and fn_def_id fields are purely informational; equality compares only id. */
[@deriving (show({with_path: false}), sexp, yojson)]
type frame = {
  id: Id.t,
  name: option(string),
  fn_def_id: option(Id.t),
};

let equal_frame = (a: frame, b: frame): bool => a.id == b.id;

/* Call context represented as a list of stack frames.
 * The head is the most recent (innermost) call. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(frame);

let equal = (a: t, b: t): bool => List.equal(equal_frame, a, b);

/* Project a call stack to ids. Prefer `equal` for equality checks; this is
 * for suffix/prefix comparisons that need a bare id list. */
let ids_of_stack = (cs: t): list(Id.t) => List.map((f: frame) => f.id, cs);

/* Prepend an application as a nameless frame — used for perspective
 * extension / pinning when looking at a call without having entered it. */
let extend = (app_id: Id.t, stack: t): t => [
  {
    id: app_id,
    name: None,
    fn_def_id: None,
  },
  ...stack,
];

/* Conceptually belongs to Sample.re, but lives here because Sample
 * depends on CallStack and observation events need it here. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type elided_value =
  | Opaque
  | Val(DHExp.t);

let add_entry = (stack: t, frame: frame): t => [frame, ...stack];

let empty: t = [];
