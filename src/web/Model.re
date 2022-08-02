open Core;
open Sexplib.Std;

type editor_model =
  | Simple(Zipper.t)
  | Study(int, list(Zipper.t));

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
  history: ActionHistory.t,
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
  show_backpack_targets: bool,
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
  history: ActionHistory.empty,
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
  show_backpack_targets: false,
  double_tap: None,
  settings: settings_init,
};

let blank = mk(Simple(empty_zipper));

let get_zipper = (model: t): Zipper.t =>
  switch (model.editor_model) {
  | Simple(zipper) => zipper
  | Study(n, zs) =>
    assert(n < List.length(zs));
    List.nth(zs, n);
  };

let put_zipper = (model: t, z: Zipper.t): editor_model =>
  switch (model.editor_model) {
  | Simple(_) => Simple(z)
  | Study(n, zs) =>
    assert(n < List.length(zs));
    Study(n, Util.ListUtil.put_nth(n, z, zs));
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
  };
