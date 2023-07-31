open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | DebugLoad
  | Scratch
  | Examples
  | Exercise;

let mode_of_string = (s: string): mode =>
  switch (s) {
  | "Scratch" => Scratch
  | "Examples" => Examples
  | "Exercise" => Exercise
  | _ => Scratch
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  captions: bool,
  secondary_icons: bool,
  statics: bool,
  dynamics: bool,
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
