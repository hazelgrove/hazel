open Util;
open ProjectorBase;

/* Reach refractor (logic half).
 *
 * Attached to any expression (a "reach point"), this asks an SMT solver for
 * inputs that make execution reach the node — or proves it unreachable (dead
 * code). The path condition is computed in ProjectorInfo.mk_info (which has
 * the whole-program statics map) and threaded in via info.reach; the web view
 * builds the SMT-LIB2 from it, runs the solver asynchronously, and dispatches
 * the interpreted SetResult. This module only holds the result.
 *
 * The outcome type is shared with TestGen, reinterpreted by the view: Unsat
 * means "unreachable (dead code)" and Sat means "reached when …". */

/* `group` controls merging: 0 = solo (per-point reachability/dead-code),
 * N≥1 = conjoined with every other group-N reach point ("one input reaching
 * all of them"). Group ids are arbitrary positive ints (the view assigns and
 * colors them dynamically); the set in use drives the chip's cycle range.
 *
 * `enabled` toggles the point like a breakpoint: a disabled point stays listed
 * but is dropped from its group's merge and is not solved (see
 * ProjectorInfo.resolve_reach). Defaulted so models serialized before this
 * field deserialize as enabled. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  [@default 0]
  group: int,
  [@default true]
  enabled: bool,
  result: option(TestGen.outcome),
};

let init_model: t = {
  group: 0,
  enabled: true,
  result: None,
};

let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
  switch (t_of_sexp(sexp)) {
  | model => model
  | exception _ => init_model
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type reach_action =
  | SetResult(TestGen.outcome)
  | SetGroup(int)
  | SetEnabled(bool)
  | Clear;

module M: Projector with type model = t and type action = reach_action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = reach_action;

  let init = (any: Language.Any.t) =>
    switch (any) {
    | Exp(_) => Some(init_model)
    | _ => None
    };

  let dynamics = false;
  let elaborate_syntax = false;

  /* Refractor: the underlying syntax stays put; result shown in the offside. */
  let placeholder = (_, _) => ProjectorCore.Shape.default;

  let update = (model: model, _info: info, action: action): model =>
    switch (action) {
    | SetResult(outcome) => {
        ...model,
        result: Some(outcome),
      }
    /* Changing group changes the constraint set, so the old result is stale.
       The view computes the next group from the set currently in use. */
    | SetGroup(group) => {
        ...model,
        group,
        result: None,
      }
    /* Enabling/disabling changes which points are merged, so any result is
       stale (the merged condition may differ). */
    | SetEnabled(enabled) => {
        ...model,
        enabled,
        result: None,
      }
    | Clear => {
        ...model,
        result: None,
      }
    };

  let error = (_, _): option(ProjectorBase.error) => None;
};
