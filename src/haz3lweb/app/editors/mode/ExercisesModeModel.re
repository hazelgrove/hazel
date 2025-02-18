open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  current: int,
  exercises: list(ExerciseModeModel.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent = {
  cur_exercise: Exercise.key,
  exercise_data: list((Exercise.key, ExerciseModeModel.persistent)),
};
