open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Projector;
open Util.OptUtil.Syntax;

let handle = (id, action): UpdateAction.t => {
  switch (action) {
  | Remove => PerformAction(Project(Remove(id)))
  | UpdateSyntax(f) => PerformAction(Project(UpdateSyntax(id, f)))
  | UpdateModel(action) =>
    //TODO(andrew)
    // print_endline("TODO: update model");
    PerformAction(
      Project(
        UpdateModel(
          id,
          p => {
            let (module P) = Projector.to_module(p);
            P.update(action);
          },
        ),
      ),
    )
  // PerformAction(Project(UpdateModel(id, x => x)));
  };
};

let to_module =
    (
      id: Id.t,
      syntax: Piece.t,
      p: Projector.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
    )
    : ProjectorViewModule.t => {
  switch (p) {
  | Fold(model) =>
    FoldView.mk(syntax, model, ~inject=a => inject(handle(id, a)))
  | Infer(model) =>
    InferView.mk(syntax, model, ~inject=a => inject(handle(id, a)))
  | Checkbox(model) =>
    CheckboxView.mk(syntax, model, ~inject=a => inject(handle(id, a)))
  | Slider(model) =>
    SliderView.mk(syntax, model, ~inject=a => inject(handle(id, a)))
  };
};

let wrap =
    (
      ~font_metrics: FontMetrics.t,
      ~measurement: Measured.measurement,
      p: Projector.t,
      clss: list(string),
      view: Node.t,
    ) =>
  div(
    ~attr=
      Attr.many([
        Attr.classes(["projector", Projector.name(p)] @ clss),
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [
      div(
        ~attr=
          Attr.many([
            Attr.classes(["projector-wrapper"] @ clss),
            JsUtil.stop_mousedown_propagation,
          ]),
        [view],
      ),
      PieceDec.convex_shard(~font_metrics, ~measurement),
    ],
  );

let view =
    (
      id: Id.t,
      ps: Map.t,
      ~syntax_map: Id.Map.t(syntax),
      ~measured: Measured.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~font_metrics,
    ) => {
  let* p = Projector.Map.find(id, ps);
  let* syntax = Id.Map.find_opt(id, syntax_map);
  let+ measurement = Measured.find_by_id(id, measured);
  let (module PV) = to_module(id, syntax, p, ~inject);
  wrap(~font_metrics, ~measurement, p, [], PV.view);
};

let indication_view =
    (
      id: Id.t,
      ps: Map.t,
      ~syntax_map: Id.Map.t(syntax),
      ~measured: Measured.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~font_metrics,
    )
    : option(Node.t) => {
  let* p = Projector.Map.find(id, ps);
  let* syntax = Id.Map.find_opt(id, syntax_map);
  let+ measurement = Measured.find_by_id(id, measured);
  let (module PV) = to_module(id, syntax, p, ~inject);
  wrap(~font_metrics, ~measurement, p, ["indicated"], PV.view);
};

let view_all =
    (
      ps: Map.t,
      ~syntax_map: Id.Map.t(syntax),
      ~measured: Measured.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~font_metrics,
    ) =>
  List.filter_map(
    ((id, _)) =>
      view(id, ps, ~syntax_map, ~measured, ~inject, ~font_metrics),
    Id.Map.bindings(ps),
  );

let indicated_proj_ed = (editor: Editor.t) => {
  let projectors = Editor.get_projectors(editor);
  //TODO(andrew): In future use z_proj instead of zipper?
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
    let+ action = PV.keymap(key);
    handle(id, action);
  };

let ci = (editor: Editor.t, ~inject: UpdateAction.t => Ui_effect.t(unit)) => {
  let* (id, p) = indicated_proj_ed(editor);
  let+ syntax = Id.Map.find_opt(id, editor.state.meta.projected.syntax_map);
  let (module PV) = to_module(id, syntax, p, ~inject);
  div(
    ~attr=Attr.classes(["projector-ci"]),
    [text(String.sub(Projector.name(p), 0, 1))],
  );
};
