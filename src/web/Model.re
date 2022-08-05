open Core;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type editor = {
  zipper: Zipper.t,
  history: ActionHistory.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type editor_model =
  | Simple(editor)
  | Study(int, list(editor))
  | School(int, list(editor));

let cell_captions = [
  "Student Implementation",
  "Student Tests",
  "Teacher Tests",
];

[@deriving (show({with_path: false}), sexp, yojson)]
type simple = (Id.t, editor);

[@deriving (show({with_path: false}), sexp, yojson)]
type study = (Id.t, int, list(editor));

[@deriving (show({with_path: false}), sexp, yojson)]
type school = (Id.t, int, list(editor));

let mk_editor = (id): editor => {
  zipper: Zipper.init(id),
  history: ActionHistory.empty,
};

let simple_init: simple = (1, mk_editor(0));

let school_init: school = (
  3,
  0,
  [mk_editor(0), mk_editor(1), mk_editor(2)],
);

let study_init: study = (
  3,
  0,
  [mk_editor(0), mk_editor(1), mk_editor(2)],
);

[@deriving (show({with_path: false}), yojson)]
type timestamp = float;

[@deriving (show({with_path: false}), sexp, yojson)]
type settings = {
  captions: bool,
  whitespace_icons: bool,
  //editor_model: editor_model,
};

let settings_init = {
  captions: true,
  whitespace_icons: false,
  //editor_model: Simple(mk_editor(0)),
};

type t = {
  editor_model,
  id_gen: IdGen.state,
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
  show_backpack_targets: bool, // current?
  double_tap: option(timestamp),
  settings,
};

let cutoff = (===);

let mk = editor_model => {
  id_gen: 1,
  editor_model,
  settings: settings_init,
  // TODO: move below to 'internals'?
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
  show_backpack_targets: false,
  double_tap: None,
};

let blank = mk(School(0, []));

let get_editor' = (editor_model: editor_model): editor =>
  switch (editor_model) {
  | Simple(editor) => editor
  | Study(n, eds) =>
    assert(n < List.length(eds));
    List.nth(eds, n);
  | School(n, eds) =>
    assert(n < List.length(eds));
    List.nth(eds, n);
  };

let get_editor = (model: t): editor => get_editor'(model.editor_model);

let put_editor = (model: t, ed: editor): editor_model =>
  switch (model.editor_model) {
  | Simple(_) => Simple(ed)
  | Study(n, eds) =>
    assert(n < List.length(eds));
    Study(n, Util.ListUtil.put_nth(n, ed, eds));
  | School(n, eds) =>
    assert(n < List.length(eds));
    School(n, Util.ListUtil.put_nth(n, ed, eds));
  };

let get_zipper' = (editor_model: editor_model): Zipper.t =>
  get_editor'(editor_model).zipper;
let get_history' = (editor_model: editor_model): ActionHistory.t =>
  get_editor'(editor_model).history;

let get_zipper = (model: t): Zipper.t => get_zipper'(model.editor_model);
let get_history = (model: t): ActionHistory.t =>
  get_history'(model.editor_model);

let current_editor = (model: t): int =>
  switch (model.editor_model) {
  | Simple(_) => 0
  | Study(n, zs) =>
    assert(n < List.length(zs));
    n;
  | School(n, zs) =>
    assert(n < List.length(zs));
    n;
  };
