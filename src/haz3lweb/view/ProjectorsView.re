open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Projector;
open Util.OptUtil.Syntax;

let to_module = (p: Projector.t): ProjectorViewModule.t =>
  switch (p) {
  | Fold(data) => FoldProjectorView.mk(data)
  | Infer(data) => InferProjectorView.mk(data)
  };

let view =
    (id: Id.t, ps: Map.t, ~measured: Measured.t, ~inject, ~font_metrics) => {
  let* p = Projector.Map.find(id, ps);
  let+ measurement = Measured.find_by_id(id, measured);
  let (module PV) = to_module(p);
  PV.normal(id, ~inject, ~font_metrics, ~measurement);
};

let indication_view =
    (id: Id.t, ps: Map.t, measured: Measured.t, ~inject, ~font_metrics)
    : option(Node.t) => {
  let* p = Projector.Map.find(id, ps);
  let+ measurement = Measured.find_by_id(id, measured);
  let (module PV) = to_module(p);
  PV.indicated(id, ~inject, ~font_metrics, ~measurement);
};

let view_all = (ps: Map.t, measured: Measured.t, ~inject, ~font_metrics) =>
  List.filter_map(
    ((id, _)) => view(id, ps, ~measured, ~inject, ~font_metrics),
    Id.Map.bindings(ps),
  );

let indicated_proj_ed = (editor: Editor.t) => {
  let projectors = Editor.get_projectors(editor);
  //TODO(andrew): use z_proj instead of zipper?
  let* id = Indicated.index(editor.state.zipper);
  let+ projector = Projector.Map.find(id, projectors);
  (id, projector);
};

let key_handler = (editor: Editor.t, key: Key.t): option(UpdateAction.t) =>
  switch (indicated_proj_ed(editor)) {
  | None => None
  | Some((id, p)) =>
    let (module PV) = to_module(p);
    PV.key_handler(id, key);
  };

let ci = (~inject as _, editor: Editor.t) => {
  let+ (_, p) = indicated_proj_ed(editor);
  let (module PV) = to_module(p);
  div(~attr=Attr.classes(["projector-ci"]), [text(PV.ci_string())]);
};
