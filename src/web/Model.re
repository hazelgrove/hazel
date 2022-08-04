open Core;
open Sexplib.Std;

/*
 school editor design:
 a stack is a list of editors
 each editor index has a name and a
 one editor can be focussed at a time,
 this determines where keyboard input goes,
 and determines cursor-info displayed
 each editor can take a list of earlier-indexed editors as includes
 that editor will be treated by eval as splice of includes+it in index-order
 each editor can be marked as hidden or visible or editable
 this determines the way it will be shown to student
 whole stack designates a list of indices to produce results for
 result/test-panel

 note: hidden prelude can leak via hole insertion

 */

//[@deriving (show({with_path: false}), sexp, yojson)]
//type mini_editor = Zipper.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type stage_names =
  | StudentAttempt
  | StudentTests;
//hidden_prelude: mini_editor,
//teacher_attempt: mini_editor,
//teacher_tests: mini_editor,
//wrong_attempts: mini_editor,

[@deriving (show({with_path: false}), sexp, yojson)]
type stage = {z: Zipper.t};

let get_stage_idx: stage_names => int =
  fun
  | StudentAttempt => 0
  | StudentTests => 1;

[@deriving (show({with_path: false}), sexp, yojson)]
type editor_model =
  | Simple(Zipper.t)
  | Study(int, list(Zipper.t))
  | School(int, list(stage));

[@deriving (show({with_path: false}), sexp, yojson)]
type school = (int, list(stage));

let school_init = (1, [{z: Zipper.init}, {z: Zipper.init}]);

[@deriving (show({with_path: false}), yojson)]
type timestamp = float;

[@deriving (show({with_path: false}), sexp, yojson)]
type settings = {
  captions: bool,
  whitespace_icons: bool,
};

let settings_init = {captions: false, whitespace_icons: false};

type t = {
  editor_model,
  id_gen: IdGen.state,
  history: ActionHistory.t, // TODO: move to editor-level?
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
  show_backpack_targets: bool, // current?
  double_tap: option(timestamp),
  settings,
};

let cutoff = (===);

let empty_zipper: Zipper.t = {
  selection: {
    focus: Left,
    content: [],
  },
  backpack: [],
  relatives: {
    siblings: ([], [Grout({id: 0, shape: Convex})]),
    ancestors: [],
  },
  caret: Outer,
  caret_col_target: 0,
};

let mk = editor_model => {
  id_gen: 1,
  editor_model,
  settings: settings_init,
  history: ActionHistory.empty,
  // TODO: move below to 'internals'?
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
  show_backpack_targets: false,
  double_tap: None,
};

let blank = mk(Simple(empty_zipper));

let get_zipper' = (editor_model: editor_model): Zipper.t =>
  switch (editor_model) {
  | Simple(zipper) => zipper
  | Study(n, zs) =>
    assert(n < List.length(zs));
    List.nth(zs, n);
  | School(n, zs) =>
    assert(n < List.length(zs));
    List.nth(zs, n).z;
  };

let get_zipper = (model: t): Zipper.t => get_zipper'(model.editor_model);

let put_zipper = (model: t, z: Zipper.t): editor_model =>
  switch (model.editor_model) {
  | Simple(_) => Simple(z)
  | Study(n, zs) =>
    assert(n < List.length(zs));
    Study(n, Util.ListUtil.put_nth(n, z, zs));
  | School(n, zs) =>
    assert(n < List.length(zs));
    School(n, Util.ListUtil.put_nth(n, {z: z}, zs));
  };

let update_zipper = (f: Zipper.state => Zipper.state, model: t): t => {
  let (z, id_gen) = f((get_zipper(model), model.id_gen));
  {...model, id_gen, editor_model: put_zipper(model, z)};
};

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
