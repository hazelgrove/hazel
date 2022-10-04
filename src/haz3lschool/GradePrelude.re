module ExerciseEnv = {
  type node = unit;
  let default = ();
  let output_header = SchoolExercise.output_header_grading;
};

module SchoolExercise = SchoolExercise.F(ExerciseEnv);

module Grading = Grading.F(ExerciseEnv);
