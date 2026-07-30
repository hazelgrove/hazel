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
 * depends on CallStack and this type is needed for app_args below. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type elided_value =
  | Opaque
  | Val(DHExp.t);

/* Argument values for function applications, keyed by app_id.
 * Each entry is a list of (call_stack_before_entering, elided_arg_value).
 * The call_stack is the stack BEFORE entering the function, so we can match
 * samples taken inside the function with their calling arguments. */
[@deriving (show({with_path: false}), sexp, yojson)]
type app_args = Id.Map.t(list((t, elided_value)));

/* Full call-stack state threaded through evaluation: the stack itself plus
 * recorded argument values for applications. */
type state = {
  stack: t,
  app_args,
};

/* Add an argument value for an application */
let add_app_arg = (state: state, app_id: Id.t, arg: elided_value): state => {
  let existing =
    Id.Map.find_opt(app_id, state.app_args) |> Option.value(~default=[]);
  {
    ...state,
    app_args:
      Id.Map.add(app_id, [(state.stack, arg), ...existing], state.app_args),
  };
};

/* Look up argument value for an application at a specific call_stack.
 * Used when creating samples for probes on Ap expressions. */
let lookup_app_arg =
    (state: state, app_id: Id.t, call_stack: t): option(elided_value) =>
  switch (Id.Map.find_opt(app_id, state.app_args)) {
  | None => None
  | Some(entries) =>
    List.find_map(
      ((stored_stack, arg)) =>
        equal(stored_stack, call_stack) ? Some(arg) : None,
      entries,
    )
  };

let add_entry = (state: state, frame: frame): state => {
  {
    ...state,
    stack: [frame, ...state.stack],
  };
};

let empty = {
  stack: [],
  app_args: Id.Map.empty,
};
