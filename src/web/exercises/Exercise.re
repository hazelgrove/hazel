/* The common interface for an exercise, across all kinds.

   Code-exercise-specific logic (points, hidden tests, stitching, etc.) lives
   in [CodeExercise.re]. Derivation- and Theorem-specific logic lives in
   [DerivationExercise.re] and [TheoremExercise.re] respectively. This module
   is just the thin dispatcher that unifies them. */

/* Sum type over all exercise kinds. An exercise file should produce a value
   of this type, tagged with the appropriate constructor, so
   [[ExerciseSettings_base.re]] can simply list them without additional
   wrapping. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Code(CodeExercise.spec)
  | Derivation(DerivationExercise.spec)
  | Theorem(TheoremExercise.spec);

let id_of = (e: t): Haz3lcore.Id.t =>
  switch (e) {
  | Code(s) => s.id
  | Derivation(s) => s.id
  | Theorem(s) => s.id
  };

let max_points_of = (e: t): int =>
  switch (e) {
  | Code(s) =>
    let {test_validation, mutation_testing, impl_grading}: CodeExercise.point_distribution =
      s.point_distribution;
    test_validation + mutation_testing + impl_grading;
  | Derivation(s) => s.max_points
  | Theorem(s) => s.max_points
  };
