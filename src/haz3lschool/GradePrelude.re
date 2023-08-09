module ExerciseEnv = {
  type node = unit;
  let default = ();
  let output_header = Exercise.output_header_grading;
};

module Exercise = Exercise.F(ExerciseEnv);

module Grading = Grading.F(ExerciseEnv);
