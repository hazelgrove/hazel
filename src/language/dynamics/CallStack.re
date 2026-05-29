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

/* Extract just the IDs from a call stack, discarding function names. */
let ids_of_stack = (cs: t): list(Id.t) => List.map((f: frame) => f.id, cs);

// This should really be defined in Sample.re
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

type t' = {
  stack: t,
  app_args /* Argument values for function applications */
};

/* Add an argument value for an application */
let add_app_arg = (state: t', app_id: Id.t, arg: elided_value): t' => {
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
    (state: t', app_id: Id.t, call_stack: t): option(elided_value) => {
  let call_stack_ids = ids_of_stack(call_stack);
  switch (Id.Map.find_opt(app_id, state.app_args)) {
  | None => None
  | Some(entries) =>
    List.find_map(
      ((stored_stack, arg)) =>
        ids_of_stack(stored_stack) == call_stack_ids ? Some(arg) : None,
      entries,
    )
  };
};

let add_entry = (state: t', frame: frame): t' => {
  {
    ...state,
    stack: [frame, ...state.stack],
  };
};

let empty = {
  stack: [],
  app_args: Id.Map.empty,
};
