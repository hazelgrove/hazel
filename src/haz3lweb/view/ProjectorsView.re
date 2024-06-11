open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Projector;
open Util.OptUtil.Syntax;

let proj_inject = (id, action: Projector.action('action)): UpdateAction.t => {
  switch (action) {
  | Remove => PerformAction(Project(Remove(id)))
  | UpdateSyntax(f) => PerformAction(Project(UpdateSyntax(id, f)))
  | Internal(_) =>
    //TODO(andrew)
    PerformAction(Project(Remove(id)))
  };
};

let to_module =
    (
      id,
      syntax: Piece.t,
      p: Projector.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
    )
    : ProjectorViewModule.t => {
  let inject = pa => inject(proj_inject(id, pa));
  switch (p) {
  | Fold(model) => FoldProjectorView.mk(syntax, model, ~inject)
  | Infer(model) => InferProjectorView.mk(syntax, model, ~inject)
  | Checkbox(model) => CheckboxProjectorView.mk(syntax, model, ~inject)
  };
};

let view =
    (
      id: Id.t,
      ps: Map.t,
      syntax_map,
      ~measured: Measured.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~font_metrics,
    ) => {
  let* p = Projector.Map.find(id, ps);
  let* syntax = Id.Map.find_opt(id, syntax_map);
  let+ measurement = Measured.find_by_id(id, measured);
  let (module PV) = to_module(id, syntax, p, ~inject);
  PV.normal(~font_metrics, ~measurement);
};

let indication_view =
    (
      id: Id.t,
      ps: Map.t,
      syntax_map,
      measured: Measured.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~font_metrics,
    )
    : option(Node.t) => {
  let* p = Projector.Map.find(id, ps);
  let* syntax = Id.Map.find_opt(id, syntax_map);
  let+ measurement = Measured.find_by_id(id, measured);
  let (module PV) = to_module(id, syntax, p, ~inject);
  PV.indicated(~font_metrics, ~measurement);
};

let view_all =
    (
      ps: Map.t,
      syntax_map,
      measured: Measured.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~font_metrics,
    ) =>
  List.filter_map(
    ((id, _)) =>
      view(id, ps, syntax_map, ~measured, ~inject, ~font_metrics),
    Id.Map.bindings(ps),
  );

let indicated_proj_ed = (editor: Editor.t) => {
  let projectors = Editor.get_projectors(editor);
  //TODO(andrew): use z_proj instead of zipper?
  let* id = Indicated.index(editor.state.zipper);
  let+ projector = Projector.Map.find(id, projectors);
  (id, projector);
};

let key_handler =
    (
      editor: Editor.t,
      key: Key.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
    )
    : option(UpdateAction.t) =>
  switch (indicated_proj_ed(editor)) {
  | None => None
  | Some((id, p)) =>
    let* syntax = Id.Map.find_opt(id, editor.state.meta.projected.syntax_map);
    let (module PV) = to_module(id, syntax, p, ~inject);
    let+ action = PV.key_handler(key);
    proj_inject(id, action);
  };

let ci = (editor: Editor.t, ~inject: UpdateAction.t => Ui_effect.t(unit)) => {
  let* (id, p) = indicated_proj_ed(editor);
  let+ syntax = Id.Map.find_opt(id, editor.state.meta.projected.syntax_map);
  let (module PV) = to_module(id, syntax, p, ~inject);
  div(~attr=Attr.classes(["projector-ci"]), [text(PV.ci_string())]);
};
