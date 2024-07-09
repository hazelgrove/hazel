open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Projector;
open Util.OptUtil.Syntax;
open Util.Web;

let update_model = (action, syntax, p) => {
  let (module P) = to_module(syntax, p);
  P.update(action);
};

let handle = (id, syntax, action: ProjectorBase.action): Action.project =>
  switch (action) {
  | Default =>
    //TODO(andrew): proper no-op
    Remove(Id.invalid)
  | Remove => Remove(id)
  | FocusInternal(selector) =>
    JsUtil.get_elem_by_selector(selector)##focus;
    Action.(FocusInternal(id));
  | Escape(selector, Left) =>
    JsUtil.get_elem_by_selector(selector)##blur;
    Escape(id, Left);
  | Escape(selector, Right) =>
    JsUtil.get_elem_by_selector(selector)##blur;
    Escape(id, Right);
  | UpdateSyntax(f) => UpdateSyntax(id, f)
  | UpdateModel(action) => UpdateModel(id, update_model(action, syntax))
  };

let backing_deco =
    (
      ~font_metrics: FontMetrics.t,
      ~measurement: Measured.measurement,
      ~syntax,
      p: Projector.t,
    ) =>
  switch (Projector.shape(p, syntax)) {
  | Inline(_) => PieceDec.convex_shard(~font_metrics, ~measurement)
  | Block(_) => div([])
  };

let wrap =
    (
      ~font_metrics: FontMetrics.t,
      ~measurement: Measured.measurement,
      ~accent: option(Projector.accent),
      ~syntax,
      p: Projector.t,
      view: Node.t,
    ) =>
  div(
    ~attrs=[
      Attr.classes(
        ["projector", Projector.name(p)] @ Projector.cls(accent),
      ),
      DecUtil.abs_style(measurement, ~font_metrics),
    ],
    [
      div(
        ~attrs=[
          JsUtil.stop_mousedown_propagation,
          Attr.classes(["projector-wrapper"]),
        ],
        [view],
      ),
      backing_deco(~font_metrics, ~measurement, ~syntax, p),
    ],
  );

let view =
    (
      id: Id.t,
      ps: Map.t,
      ~syntax_map: Id.Map.t(syntax),
      ~measured: Measured.t,
      ~inject: Action.project => Ui_effect.t(unit),
      ~font_metrics,
      ~accent: option(Projector.accent),
    )
    : option(Node.t) => {
  let* p = Projector.Map.find(id, ps);
  let* syntax = Id.Map.find_opt(id, syntax_map);
  let+ measurement = Measured.find_by_id(id, measured);
  let (module P) = to_module(syntax, p);
  let inject = a => handle(id, syntax, a) |> inject;
  wrap(
    ~font_metrics,
    ~measurement,
    ~accent,
    ~syntax,
    p,
    P.view(~inject, accent),
  );
};

let view_all =
    (
      ps: Map.t,
      ~syntax_map: Id.Map.t(syntax),
      ~measured: Measured.t,
      ~inject: Action.project => Ui_effect.t(unit),
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

let key_handler = (editor: Editor.t, key: Key.t): option(Action.project) =>
  switch (indicated_proj_ed(editor)) {
  | None => None
  | Some((id, p)) =>
    let* syntax = Id.Map.find_opt(id, editor.state.meta.projected.syntax_map);
    let (module P) = to_module(syntax, p);
    let* (_, d, _) = Indicated.piece(editor.state.zipper);
    let+ action = P.keymap(d, key);
    handle(id, syntax, action);
  };

let option_view = (name, n) =>
  option(
    ~attrs=n == name ? [Attr.create("selected", "selected")] : [],
    [text(n)],
  );

let applicable_projectors = (ci: Info.t): list(Projector.kind) =>
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
  @ [Projector.Fold]
  @ (
    switch (ci) {
    | InfoExp(_)
    | InfoPat(_) => [Infer]
    | _ => []
    }
  );

let toggle_projector = (active, id, ci): Action.project =>
  active || applicable_projectors(ci) == []
    ? Remove(id) : SetIndicated(List.hd(applicable_projectors(ci)));

let toggle_view = (~inject, ci, id, active: bool) =>
  div(
    ~attrs=[
      clss(["toggle-switch"] @ (active ? ["active"] : [])),
      Attr.on_click(_ => inject(toggle_projector(active, id, ci))),
    ],
    [
      div(
        ~attrs=[clss(["toggle-knob"])],
        [
          Node.create(
            "img",
            ~attrs=[Attr.src("img/noun-fold-1593402.svg")],
            [],
          ),
        ],
      ),
    ],
  );

let currently_selected = editor =>
  option_view(
    switch (kind(editor)) {
    | None => "Fold"
    | Some(k) => Projector.name_of_kind(k)
    },
  );

let panel = (~inject, editor: Editor.t, ci: Info.t) => {
  div(
    ~attrs=[Attr.id("projectors")],
    [
      toggle_view(~inject, ci, id(editor), kind(editor) != None),
      Node.select(
        ~attrs=[
          Attr.on_change((_, name) =>
            inject(SetIndicated(Projector.of_name(name)))
          ),
        ],
        applicable_projectors(ci)
        |> List.map(Projector.name_of_kind)
        |> List.map(currently_selected(editor)),
      ),
    ],
  );
};
