open Haz3lcore;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type editors =
  | Simple(Editor.t)
  | Study(int, list(Editor.t))
  | School(SchoolExercise.state);

[@deriving (show({with_path: false}), sexp, yojson)]
type simple = (Id.t, Editor.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type study = (Id.t, int, list(Editor.t));

[@deriving (show({with_path: false}), sexp, yojson)]
type school = (Id.t, SchoolExercise.state);

[@deriving (show({with_path: false}), yojson)]
type timestamp = float;

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Simple
  | Study
  | School;

[@deriving (show({with_path: false}), sexp, yojson)]
type settings = {
  captions: bool,
  whitespace_icons: bool,
  statics: bool,
  dynamics: bool,
  context_inspector: bool,
  student: bool,
  mode,
};

let settings_init = {
  captions: true,
  whitespace_icons: false,
  statics: true,
  dynamics: true,
  context_inspector: false,
  student: true,
  mode: Simple,
};

type t = {
  editors,
  id_gen: IdGen.state,
  settings,
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
  show_backpack_targets: bool,
  double_tap: option(timestamp),
  clipboard: string,
  mousedown: bool,
};

let cutoff = (===);

let mk = editors => {
  id_gen: 1,
  editors,
  settings: settings_init,
  // TODO: move below to 'io_state'?
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
  show_backpack_targets: false,
  double_tap: None,
  clipboard: ",",
  mousedown: false,
};

let blank = mk(Study(0, []));

let get_editor' = (editors: editors): Editor.t =>
  switch (editors) {
  | Simple(editor) => editor
  | Study(n, eds) =>
    assert(n < List.length(eds));
    List.nth(eds, n);
  | School(state) => state.editor
  };

let get_editor = (model: t): Editor.t => get_editor'(model.editors);

let put_editor = (model: t, editor: Editor.t): editors =>
  switch (model.editors) {
  | Simple(_) => Simple(editor)
  | Study(n, eds) =>
    assert(n < List.length(eds));
    Study(n, Util.ListUtil.put_nth(n, editor, eds));
  | School(state) => School({...state, editor})
  };

let get_zipper' = (editors: editors): Zipper.t =>
  get_editor'(editors).state.zipper;
let get_zipper = (model: t): Zipper.t => get_zipper'(model.editors);
let get_history = (model: t): Editor.History.t =>
  get_editor'(model.editors).history;
let get_touched = (model: t): Touched.t =>
  get_editor'(model.editors).state.meta.touched;

let current_editor = (model: t): int =>
  switch (model.editors) {
  | Simple(_) => 0
  | Study(n, zs) =>
    assert(n < List.length(zs));
    n;
  | School(_) => 0
  };

let num_editors = (model: t): int =>
  switch (model.editors) {
  | Simple(_) => 1
  | Study(_, eds) => List.length(eds)
  | School(_) => 1
  };

let simple_init: simple = (1, Editor.empty(0));
