open Util;

/*
 ModelResults is used to store the results of
 evaluations requested by the current editor mode,
 with the key distinguishing these requests.

 See the SchoolExercise module for an example.
 */
module Key = {
  include String;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = string;
};

module M = Util.MapUtil.Make(Key);
include M;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = M.t(ModelResult.t);

let init_eval = (ds: list((Key.t, DHExp.t))): t =>
  ds |> List.to_seq |> of_seq |> map(ModelResult.init_eval);

let update_elabs = (~settings) =>
  List.fold_right(((k, elab), acc) =>
    update(
      k,
      v =>
        Some(
          v
          |> Option.value(~default=ModelResult.NoElab)
          |> ModelResult.update_elab(~settings, elab),
        ),
      acc,
    )
  );

let lookup = (results: t, key: Key.t) => find_opt(key, results);

let run_pending = (~settings) => M.map(ModelResult.run_pending(~settings));

let timeout_all = map(ModelResult.timeout);

let advance_evaluator_result =
    (results: t, (key: Key.t, elab: DHExp.t))
    : option((Key.t, ModelResult.t)) =>
  switch (lookup(results, key)) {
  | Some(Stepper(_)) => None
  | Some(Evaluation({evaluation: previous, _})) =>
    Some((key, Evaluation({elab, evaluation: ResultPending, previous})))
  | Some(NoElab)
  | None =>
    Some((
      key,
      Evaluation({elab, evaluation: ResultPending, previous: ResultPending}),
    ))
  };

let stepper_result_opt =
    ((key: Key.t, r: ModelResult.t)): option((Key.t, ModelResult.t)) =>
  switch (r) {
  | Stepper(_) => Some((key, r))
  | _ => None
  };

let to_evaluate = (results: t, elabs: list((Key.t, DHExp.t))): t =>
  elabs
  |> List.filter_map(advance_evaluator_result(results))
  |> List.to_seq
  |> of_seq;

let to_step = (results: t): t =>
  bindings(results)
  |> List.filter_map(stepper_result_opt)
  |> List.to_seq
  |> of_seq;
