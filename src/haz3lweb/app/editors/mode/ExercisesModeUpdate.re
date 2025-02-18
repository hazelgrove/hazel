open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | SwitchExercise(int)
  | Exercise(ExerciseModeUpdate.t)
  | ExportModule
  | ExportSubmission
  | ExportTransitionary
  | ExportGrading;
