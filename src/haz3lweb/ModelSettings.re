open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type live_inspector = {
  on: bool,
  use_cursor: bool,
  show_fns_in_env: bool,
  ids: list(Id.t),
  cur_env_idx: int,
  cur_env: ProbeMap.dhexp_env,
};

let live_inspector_init = {
  on: false,
  use_cursor: true,
  show_fns_in_env: false,
  ids: [],
  cur_env_idx: 0,
  cur_env: [],
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  live_inspector,
  captions: bool,
  secondary_icons: bool,
  statics: bool,
  dynamics: bool,
  async_evaluation: bool,
  context_inspector: bool,
  instructor_mode: bool,
  benchmark: bool,
  mode: Editors.mode,
};

let init = {
  live_inspector: live_inspector_init,
  captions: true,
  secondary_icons: false,
  statics: true,
  dynamics: true,
  async_evaluation: false,
  context_inspector: false,
  instructor_mode: SchoolSettings.show_instructor,
  benchmark: false,
  mode: Editors.Scratch,
};

let init_debug = {...init, mode: Editors.DebugLoad};

let fix_instructor_mode = settings =>
  if (settings.instructor_mode && !SchoolSettings.show_instructor) {
    {...settings, instructor_mode: false};
  } else {
    settings;
  };
