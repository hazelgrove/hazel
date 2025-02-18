[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | SwitchMode(EditorsModel.mode)
  // Scratch & Documentation
  | Scratch(ScratchModeUpdate.t)
  // Exercises
  | Exercises(ExercisesModeUpdate.t);
