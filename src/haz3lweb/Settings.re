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

module Evaluation = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    show_case_clauses: bool,
    show_fn_bodies: bool,
    show_casts: bool,
  };

  let init = {
    show_case_clauses: true,
    show_fn_bodies: true,
    show_casts: true,
  };
};
