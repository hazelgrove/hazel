[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Scratch
  | Documentation
  | Exercises;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Scratch(ScratchModeModel.t)
  | Documentation(ScratchModeModel.t)
  | Exercises(ExercisesModeModel.t);
