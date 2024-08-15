open Haz3lcore;
open Util;

module F = (ExerciseEnv: Exercise.ExerciseEnv) => {
  open Exercise.F(ExerciseEnv);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type percentage = float;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type points = float;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type score = (points, points);

  let score_of_percent = (percent, max_points) => {
    let max_points = float_of_int(max_points);
    (percent *. max_points, max_points);
  };

  module DerivationReport = {
    type t = {verify_results: Util.Tree.p(bool)};

    let mk = (~verify_results: Util.Tree.p(bool)) => {
      verify_results;
    };

    let get_judgement = ({result, _}: DynamicsItem.t) =>
      switch (result) {
      | Evaluation({
          evaluation:
            ResultOk({result: BoxedValue({term: Judgement(j), _}), _}),
          _,
        }) =>
        Ok(j)
      | Evaluation({evaluation: ResultOk({result: BoxedValue(_), _}), _}) =>
        Error("E-249")
      | Evaluation(_) => Error("Pending")
      | Stepper(_) => Error("E-251")
      | NoElab => Error("E-252")
      };

    let get_judgement2 = ({result, _}: DynamicsItem.t) =>
      switch (result) {
      | Evaluation({
          evaluation:
            ResultOk({result: BoxedValue({term: Judgement(j), _}), _}),
          _,
        }) =>
        Some(j)
      | _ => None
      };

    let verify_single =
        (
          concl: DynamicsItem.t,
          rule: Derivation.Rule.t,
          prems: list(DynamicsItem.t),
        ) => {
      let concl = get_judgement(concl);
      let prems = List.map(get_judgement2, prems) |> List.filter_map(Fun.id);
      switch (concl, prems) {
      | (Ok(concl), prems) => DerivationError.verify(rule, concl, prems)
      | (Error(e), _) => Error(DerivationError.External(e))
      };
    };
  };

  module GradingReport = {
    type t = {derivation_report: DerivationReport.t};

    let mk = (eds: 'a, ~stitched_dynamics: Proof.stitched(DynamicsItem.t)) => {
      // derivation_report:
      //   DerivationReport.mk(~verify_results=stitched_dynamics);
      ignore(eds);
      ignore(stitched_dynamics);
      {
        derivation_report: {
          verify_results: Util.Tree.init(Fun.const(true)),
        },
      };
    };

    let overall_score = (_: t): score => {
      (99., 100.);
    };
  };
};
