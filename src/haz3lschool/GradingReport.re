open Haz3lcore;

module F = (ExerciseEnv: Exercise.ExerciseEnv) => {
  open Exercise.F(ExerciseEnv);
  module ProgrammingGrade = ProgrammingGrade.F(ExerciseEnv);
  module ProofGrade = ProofGrade.F(ExerciseEnv);

  type t =
    | Programming(ProgrammingGrade.GradingReport.t)
    | Proof(ProofGrade.GradingReport.t);

  let mk =
      (eds: model(Editor.t), ~stitched_dynamics: stitched(DynamicsItem.t)) =>
    switch (eds, stitched_dynamics) {
    | (Programming(eds), Programming(stitched_dynamics)) =>
      Programming(ProgrammingGrade.GradingReport.mk(eds, ~stitched_dynamics))
    | (Proof(eds), Proof(stitched_dynamics)) =>
      Proof(ProofGrade.GradingReport.mk(eds, ~stitched_dynamics))
    | _ => failwith("Impossible")
    };

  let overall_score = report =>
    switch (report) {
    | Programming(report) =>
      ProgrammingGrade.GradingReport.overall_score(report)
    | Proof(report) => ProofGrade.GradingReport.overall_score(report)
    };
};
