open Util;
open ProjectorBase;

/* Test input generation projector (logic half).
 *
 * Attached to a boolean expression, this projector treats the expression as
 * a constraint over its free variables and asks an SMT solver for a
 * satisfying assignment — i.e. a test input that makes the predicate true.
 * The actual (asynchronous) solve happens in the web view, which builds the
 * SMT-LIB2 with Haz3lcore.TestGen, runs the z3-solver WASM backend, and
 * dispatches SetResult when it completes. This module only holds the result.
 *
 * The model/action types live at file level (outside the sealed module) so
 * the web view (TestGenProjView) can reuse them, matching FoldProj. */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {result: option(TestGen.outcome)};

let init_model: t = {result: None};

/* Deserialization failures reset to the empty result — the model is pure
 * transient UI state. */
let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
  switch (t_of_sexp(sexp)) {
  | model => model
  | exception _ => init_model
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type testgen_action =
  | SetResult(TestGen.outcome)
  | Clear;

module M: Projector with type model = t and type action = testgen_action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = testgen_action;

  let init = (any: Language.Any.t) =>
    switch (any) {
    | Exp(_) => Some(init_model)
    | _ => None
    };

  let dynamics = false;
  let elaborate_syntax = false;

  /* Refractor: doesn't reserve inline space — the underlying syntax renders
     normally and the generated inputs are shown in an offside decoration. */
  let placeholder = (_, _) => ProjectorCore.Shape.default;

  let update = (_model: model, _info: info, action: action): model =>
    switch (action) {
    | SetResult(outcome) => {result: Some(outcome)}
    | Clear => {result: None}
    };

  let error = (_, _): option(ProjectorBase.error) => None;
};
