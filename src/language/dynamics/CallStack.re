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
 * depends on CallStack and this type is needed for app_data below. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type elided_value =
  | Opaque
  | Val(DHExp.t);

/* Arg/frame records for function applications, keyed by app_id.
 * Each entry is (call_stack_before_entering, elided_arg_value, call_frame).
 * The call_stack is the stack BEFORE entering the function, so we can match
 * samples taken inside the function with their calling arguments. The frame
 * carries the invoked fn's resolved fn_def_id, which is only observable at
 * the moment the Ap steps — this is the channel to the probe Sample minted
 * later. Recorded only for probe targets. */
[@deriving (show({with_path: false}), sexp, yojson)]
type app_data = Id.Map.t(list((t, elided_value, frame)));

/* Full call-stack state threaded through evaluation: the stack itself plus
 * recorded argument values for applications. The app_data map is a shared
 * ref rather than threaded back up: an Ap id re-evaluated via cast
 * distribution (same id, deeper recursion) must see recordings made during
 * sub-evaluation, which the threaded stack does not propagate. */
type state = {
  stack: t,
  app_data: ref(app_data),
};

/* Add an argument value + call frame for an application */
let add_app_data =
    (state: state, app_id: Id.t, arg: elided_value, frame: frame): state => {
  let existing =
    Id.Map.find_opt(app_id, state.app_data^) |> Option.value(~default=[]);
  state.app_data :=
    Id.Map.add(
      app_id,
      [(state.stack, arg, frame), ...existing],
      state.app_data^,
    );
  state;
};

/* Look up argument value + call frame for an application at a specific
 * call_stack. Used when creating samples for probes on Ap expressions. */
let lookup_app =
    (state: state, app_id: Id.t, call_stack: t)
    : option((elided_value, frame)) =>
  switch (Id.Map.find_opt(app_id, state.app_data^)) {
  | None => None
  | Some(entries) =>
    List.find_map(
      ((stored_stack, arg, frame)) =>
        equal(stored_stack, call_stack) ? Some((arg, frame)) : None,
      entries,
    )
  };

let add_entry = (state: state, frame: frame): state => {
  {
    ...state,
    stack: [frame, ...state.stack],
  };
};

/* Fresh state per evaluation run — the app_data ref must not be shared
 * across runs. */
let empty = (): state => {
  stack: [],
  app_data: ref(Id.Map.empty),
};
