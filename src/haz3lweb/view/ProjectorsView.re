open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Projector;
open Util.OptUtil.Syntax;
open Util.Web;

let update_model = (action, syntax, p) => {
  let (module P) = Projector.to_module(syntax, p);
  P.update(action);
};

let handle = (id, syntax, action): list(UpdateAction.t) => {
  switch (action) {
  | FocusInternal(selector) =>
    JsUtil.get_elem_by_selector(selector)##focus;
    /* Note: jumping her normalizes position, so when exiting
     * we know we're intially to the left and can move or not accordingly */
    [
      PerformAction(Jump(TileId(id))),
      PerformAction(Project(SetKeyDispatch(id, true))),
    ];
  | Escape(selector, Left) =>
    JsUtil.get_elem_by_selector(selector)##blur;
    [
      PerformAction(Project(SetKeyDispatch(id, false))),
      PerformAction(Move(Local(Right(ByToken)))),
    ];
  | Escape(selector, Right) =>
    JsUtil.get_elem_by_selector(selector)##blur;
    [PerformAction(Project(SetKeyDispatch(id, false)))];
  | Default =>
    //TODO(andrew): proper no-op
    []
  | Remove => [PerformAction(Project(Remove(id)))]
  | UpdateSyntax(f) => [PerformAction(Project(UpdateSyntax(id, f)))]
  | UpdateModel(action) => [
      PerformAction(
        Project(UpdateModel(id, update_model(action, syntax))),
      ),
    ]
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
    FoldView.mk(syntax, model, ~inject=a =>
      Effect.Many(List.map(inject, handle(id, syntax, a)))
    )
  | Infer(model) =>
    InferView.mk(syntax, model, ~inject=a =>
      Effect.Many(List.map(inject, handle(id, syntax, a)))
    )
  | Checkbox(model) =>
    CheckboxView.mk(syntax, model, ~inject=a =>
      Effect.Many(List.map(inject, handle(id, syntax, a)))
    )
  | Slider(model) =>
    SliderView.mk(syntax, model, ~inject=a =>
      Effect.Many(List.map(inject, handle(id, syntax, a)))
    )
  | TextArea(model) =>
    TextAreaView.mk(syntax, model, ~inject=a =>
      Effect.Many(List.map(inject, handle(id, syntax, a)))
    )
  };
};

let wrap = //TODO(andrew): cleanup params
    (
      ~inject as _,
      ~id as _,
      ~font_metrics: FontMetrics.t,
      ~measurement: Measured.measurement,
      ~accent: option(ProjectorViewModule.accent),
      ~syntax,
      p: Projector.t,
      view: Node.t,
    ) =>
  div(
    ~attr=
      Attr.many([
        // JsUtil.stop_mousedown_propagation,
        Attr.classes(
          ["projector", Projector.name(p)] @ ProjectorViewModule.cls(accent),
        ),
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [
      div(
        ~attr=
          Attr.many([
            JsUtil.stop_mousedown_propagation,
            Attr.classes(["projector-wrapper"]),
            // Attr.on_mousedown(_ => {
            //   print_endline("WRAPPPER");
            //   inject(Update.PerformAction(Jump(TileId(id))));
            // }),
          ]),
        [view],
      ),
      //TODO(andrew): document
      switch (Projector.shape(p, syntax)) {
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
      ~accent: option(ProjectorViewModule.accent),
    )
    : option(Node.t) => {
  let* p = Projector.Map.find(id, ps);
  let* syntax = Id.Map.find_opt(id, syntax_map);
  let+ measurement = Measured.find_by_id(id, measured);
  let (module PV) = to_module(id, syntax, p, ~inject);
  wrap(
    ~font_metrics,
    ~inject,
    ~id,
    ~measurement,
    ~accent,
    ~syntax,
    p,
    PV.view(accent),
  );
};

let bdfg = z =>
  switch (Indicated.piece(z)) {
  | Some((_, d, _)) => Some(d)
  | None => None
  };

let view_all =
    (
      ps: Map.t,
      ~syntax_map: Id.Map.t(syntax),
      ~measured: Measured.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~font_metrics,
      ~accent,
    ) =>
  List.filter_map(
    ((id, _)) =>
      view(
        id,
        ps,
        ~syntax_map,
        ~measured,
        ~inject,
        ~font_metrics,
        ~accent=
          switch (accent) {
          | Some((ind_id, ind_d)) when ind_id == id => Some(ind_d)
          | _ => None
          },
      ),
    Id.Map.bindings(ps),
  );

let indicated_proj_z = (z: Zipper.t) => {
  let* id = Indicated.index(z);
  let+ projector = Projector.Map.find(id, z.projectors);
  (id, projector);
};

let indicated_proj_ed = (editor: Editor.t) =>
  //TODO(andrew): In future use z_proj instead of zipper?
  indicated_proj_z(editor.state.zipper);

let kind = (editor: Editor.t) => {
  let+ (_, p) = indicated_proj_ed(editor);
  Projector.kind(p);
};

let shape = (z: Zipper.t, syntax) => {
  let+ (_, p) = indicated_proj_z(z);
  Projector.shape(p, syntax);
};

let id = (editor: Editor.t) => {
  switch (indicated_proj_ed(editor)) {
  | Some((id, _)) => id
  | None => Id.invalid
  };
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

let key_handler =
    (
      editor: Editor.t,
      key: Key.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
    )
    : option(list(UpdateAction.t)) =>
  switch (indicated_proj_ed(editor)) {
  | None => None
  | Some((id, p)) =>
    let* syntax = Id.Map.find_opt(id, editor.state.meta.projected.syntax_map);
    let (module PV) = to_module(id, syntax, p, ~inject);
    let* (_, d, _) = Indicated.piece(editor.state.zipper);
    let+ action = PV.keymap(d, key);
    handle(id, syntax, action);
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
    switch (kind(editor)) {
    | None => "Fold"
    | Some(k) => Projector.name_(k)
    },
  );

let panel = (~inject, editor: Editor.t, ci: Info.t) => {
  div(
    ~attr=Attr.id("projectors"),
    [
      toggle_view(~inject, ci, id(editor), kind(editor) != None),
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
