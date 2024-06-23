open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Projector;
open Util.OptUtil.Syntax;
open Util.Web;

let rec handle = (id, action): UpdateAction.t => {
  switch (action) {
  | Focus =>
    //TODO(andrew): end up on nearest side
    PerformAction(Jump(TileId(id)))
  | Default =>
    //TODO(andrew): no-op
    PerformAction(Project(Remove(Id.invalid)))
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
  | Seq(a1, _a2) => handle(id, a1) //TODO
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
  | TextArea(model) =>
    TextAreaView.mk(syntax, model, ~inject=a => inject(handle(id, a)))
  };
};

let wrap =
    (
      ~inject as _,
      ~id as _,
      ~font_metrics: FontMetrics.t,
      ~measurement: Measured.measurement,
      p: Projector.t,
      clss: list(string),
      view: Node.t,
    ) =>
  div(
    ~attr=
      Attr.many([
        JsUtil.stop_mousedown_propagation,
        Attr.classes(["projector", Projector.name(p)] @ clss),
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [
      div(
        ~attr=
          Attr.many([
            Attr.classes(["projector-wrapper"] @ clss),
            // Attr.on_mousedown(_ => {
            //   print_endline("WRAPPPER");
            //   inject(Update.PerformAction(Jump(TileId(id))));
            // }),
          ]),
        [view],
      ),
      //TODO(andrew): document
      switch (Projector.shape(p)) {
      | Inline(_) => PieceDec.convex_shard(~font_metrics, ~measurement)
      | Block(_) => div([])
      },
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
  wrap(~inject, ~id, ~font_metrics, ~measurement, p, [], PV.view);
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
  wrap(~font_metrics, ~inject, ~id, ~measurement, p, ["indicated"], PV.view);
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

let cur = (editor: Editor.t) => {
  let+ (_id, p) = indicated_proj_ed(editor);
  Projector.kind(p);
};

let id = (editor: Editor.t) => {
  switch (indicated_proj_ed(editor)) {
  | None => Id.invalid
  | Some((id, _)) => id
  };
};

let option_view = (name, n) =>
  option(
    ~attr=n == name ? Attr.create("selected", "selected") : Attr.many([]),
    [text(n)],
  );

let set = (k: Projector.kind) =>
  Update.PerformAction(Project(SetIndicated(k)));

let remove = (id: Id.t) => Update.PerformAction(Project(Remove(id)));

let applicable_projectors = (ci: Info.t) =>
  (
    switch (Info.cls_of(ci)) {
    | Exp(Bool)
    | Pat(Bool) => [Checkbox]
    | Exp(Int)
    | Pat(Int) => [Slider]
    | Exp(String)
    | Pat(String) => [TextArea]
    | _ => []
    }
  )
  @ [Fold]
  @ (
    switch (ci) {
    | InfoExp(_)
    | InfoPat(_) => [Infer]
    | _ => []
    }
  );

let toggle_view = (~inject, ci, id, active: bool) =>
  div(
    ~attr=
      Attr.many([
        clss(["toggle-switch"] @ (active ? ["active"] : [])),
        Attr.on_click(_ =>
          inject(
            active
              ? remove(id)
              : applicable_projectors(ci) != []
                  ? set(List.hd(applicable_projectors(ci))) : remove(id),
          )
        ),
      ]),
    [
      div(
        ~attr=clss(["toggle-knob"]),
        [
          Node.create("img", ~attr=Attr.src("img/noun-fold-1593402.svg"), []),
        ],
      ),
    ],
  );

let currently_selected = editor =>
  option_view(
    switch (cur(editor)) {
    | None => "Fold"
    | Some(k) => Projector.name_(k)
    },
  );

let panel = (~inject, editor: Editor.t, ci: Info.t) => {
  div(
    ~attr=Attr.id("projectors"),
    [
      toggle_view(~inject, ci, id(editor), cur(editor) != None),
      Node.select(
        ~attr=
          Attr.many([
            Attr.on_change((_, name) =>
              inject(set(Projector.of_name(name)))
            ),
          ]),
        applicable_projectors(ci)
        |> List.map((k: Projector.kind) => Projector.name_(k))
        |> List.map(currently_selected(editor)),
      ),
    ],
  );
};
