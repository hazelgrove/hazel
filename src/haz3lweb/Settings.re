open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | DebugLoad
  | Scratch
  | Documentation
  | Exercises;

let mode_of_string = (s: string): mode =>
  switch (s) {
  | "Scratch" => Scratch
  | "Documentation" => Documentation
  | "Exercises" => Exercises
  | _ => failwith("mode_of_string: unknown mode:" ++ s)
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  captions: bool,
  secondary_icons: bool,
  core: Haz3lcore.CoreSettings.t,
  async_evaluation: bool,
  context_inspector: bool,
  instructor_mode: bool,
  benchmark: bool,
  mode,
};

let fix_instructor_mode = settings =>
  if (settings.instructor_mode && !ExerciseSettings.show_instructor) {
    {...settings, instructor_mode: false};
  } else {
    settings;
  };
