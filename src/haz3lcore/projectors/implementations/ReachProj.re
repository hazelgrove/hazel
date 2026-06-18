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
 * all of them"). Cycled via the group chip in the view. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  [@default 0]
  group: int,
  result: option(TestGen.outcome),
};

let init_model: t = {
  group: 0,
  result: None,
};

/* Number of merge groups offered by the chip (plus solo = 0). */
let num_groups = 5;

let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
  switch (t_of_sexp(sexp)) {
  | model => model
  | exception _ => init_model
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type reach_action =
  | SetResult(TestGen.outcome)
  | CycleGroup
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
    /* Cycling group changes the constraint set, so the old result is stale. */
    | CycleGroup => {
        group: (model.group + 1) mod (num_groups + 1),
        result: None,
      }
    | Clear => {
        ...model,
        result: None,
      }
    };

  let error = (_, _): option(ProjectorBase.error) => None;
};
