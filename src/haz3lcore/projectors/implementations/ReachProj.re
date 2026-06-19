open Util;
open ProjectorBase;

/* Reach refractor (logic half).
 *
 * Attached to any expression (a "reach point"), this asks an SMT solver for
 * inputs that make execution reach the node — or proves it unreachable (dead
 * code). The node's own (solo) path condition is computed in
 * ProjectorInfo.mk_info and threaded in via info.reach; merge-group conditions
 * are assembled in the Reach sidebar (which sees every reach point) by
 * conjoining members' solo conditions. This module only holds membership and
 * the solved outcomes.
 *
 * The outcome type is shared with TestGen, reinterpreted by the view: Unsat
 * means "unreachable (dead code)" and Sat means "reached when …". */

/* A reach point can belong to several merge groups at once. `groups` is the
 * set of group ids it is in (positive ints, colored dynamically by the view);
 * empty = no groups. Each group N conjoins all of its members ("one input
 * reaching all of them").
 *
 * `enabled` toggles the point like a breakpoint: a disabled point stays listed
 * but is dropped from every group's merge and is not solved.
 *
 * `results` holds the latest solved outcome per group the point participates
 * in, keyed by group id; key 0 is the point's own (solo) outcome. Fields are
 * defaulted so models serialized before they existed still deserialize. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  [@default []]
  groups: list(int),
  [@default true]
  enabled: bool,
  [@default []]
  results: list((int, TestGen.outcome)),
};

let init_model: t = {
  groups: [],
  enabled: true,
  results: [],
};

let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
  switch (t_of_sexp(sexp)) {
  | model => model
  | exception _ => init_model
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type reach_action =
  /* (group, outcome); group 0 = the point's own solo outcome */
  | SetResult(int, TestGen.outcome)
  /* store several (group, outcome) pairs at once (one solve of the point and
     all of its groups), so concurrent solves can't clobber each other */
  | SetResults(list((int, TestGen.outcome)))
  /* add/remove this point from a group */
  | ToggleGroup(int)
  | SetEnabled(bool)
  | ClearResults;

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
    /* Store/replace the outcome for one group (0 = solo). */
    | SetResult(group, outcome) => {
        ...model,
        results: [
          (group, outcome),
          ...List.remove_assoc(group, model.results),
        ],
      }
    /* Store/replace several outcomes at once. */
    | SetResults(pairs) => {
        ...model,
        results:
          List.fold_left(
            (acc, (group, outcome)) =>
              [(group, outcome), ...List.remove_assoc(group, acc)],
            model.results,
            pairs,
          ),
      }
    /* Toggle membership. Changing groups makes the group solutions stale, so
       drop them; the solo outcome (key 0) is unaffected and kept. */
    | ToggleGroup(group) => {
        ...model,
        groups:
          List.mem(group, model.groups)
            ? List.filter(g => g != group, model.groups)
            : List.sort(compare, [group, ...model.groups]),
        results: List.filter(((k, _)) => k == 0, model.results),
      }
    /* Enabling/disabling changes every group merge it is in, so all solved
       outcomes are stale. */
    | SetEnabled(enabled) => {
        ...model,
        enabled,
        results: [],
      }
    | ClearResults => {
        ...model,
        results: [],
      }
    };

  let error = (_, _): option(ProjectorBase.error) => None;
};
