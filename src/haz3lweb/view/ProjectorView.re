open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Projector;
open Util.OptUtil.Syntax;
open Util.Web;

let update_model = (action: ProjectorBase.inner_action, p) => {
  let (module P) = to_module(p);
  P.update(action);
};

let handle = (id, action: ProjectorBase.action): Action.project =>
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
  | UpdateModel(action) => UpdateModel(id, update_model(action))
  };

let backing_deco =
    (
      ~font_metrics: FontMetrics.t,
      ~measurement: Measured.measurement,
      ~info,
      p: Projector.t,
    ) =>
  switch (Projector.shape(p, info)) {
  | Inline(_) => PieceDec.convex_shard(~font_metrics, ~measurement)
  | Block(_) => div([])
  };

let cls = (indicated: option(status), selected) =>
  (selected ? ["selected"] : [])
  @ (
    switch (indicated) {
    | Some(Indicated(Left)) => ["indicated", "left"]
    | Some(Indicated(Right)) => ["indicated", "right"]
    | None => []
    }
  );

let view_inner =
    (
      ~font_metrics: FontMetrics.t,
      ~measurement: Measured.measurement,
      ~status: option(Projector.status),
      ~info: info,
      ~selected: bool,
      p: Projector.t,
      view: Node.t,
    ) => {
  let fudge = selected ? PieceDec.selection_fudge : DecUtil.fzero;
  div(
    ~attrs=[
      Attr.classes(
        ["projector", Projector.name(p)] @ cls(status, selected),
      ),
      DecUtil.abs_style(measurement, ~fudge, ~font_metrics),
    ],
    [
      div(
        ~attrs=[
          JsUtil.stop_mousedown_propagation,
          Attr.classes(["projector-wrapper"]),
        ],
        [view],
      ),
      backing_deco(~font_metrics, ~measurement, ~info, p),
    ],
  );
};

let view_setup =
    (
      id: Id.t,
      ~meta: Editor.Meta.t,
      ~inject: Action.project => Ui_effect.t(unit),
      ~font_metrics,
      ~status: option(Projector.status),
    )
    : option(Node.t) => {
  let* p = Projector.Map.find(id, meta.projected.z.projectors);
  let* syntax = Id.Map.find_opt(id, meta.projected.syntax_map);
  let ci = Id.Map.find_opt(id, meta.statics.info_map);
  let info = {ci, syntax, status};
  let+ measurement = Measured.find_by_id(id, meta.projected.measured);
  let (module P) = to_module(p);
  let inject = a => handle(id, a) |> inject;
  view_inner(
    ~font_metrics,
    ~measurement,
    ~status,
    ~info,
    ~selected=List.mem(id, meta.selection_ids),
    p,
    P.view(~info, ~inject),
  );
};

let status_and_id = (z: ZipperBase.t) =>
  //TODO(andrew): add Selected, Focused (maybe)
  switch (Indicated.piece(z)) {
  | Some((p, d, _)) => Some((Piece.id(p), Indicated(d)))
  | None => None
  };

let status = (z: ZipperBase.t): option(status) =>
  switch (status_and_id(z)) {
  | Some((_, status)) => Some(status)
  | None => None
  };

let view_all =
    (
      ~meta: Editor.Meta.t,
      ~inject: Action.project => Ui_effect.t(unit),
      ~font_metrics,
    ) =>
  List.filter_map(
    ((id, _)) => {
      view_setup(
        id,
        ~meta,
        ~inject,
        ~font_metrics,
        ~status=
          switch (status_and_id(meta.projected.z)) {
          | Some((ind_id, ind_d)) when ind_id == id => Some(ind_d)
          | _ => None
          },
      )
    },
    Id.Map.bindings(meta.projected.z.projectors),
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

let shape = (z: Zipper.t, syntax): option(shape) => {
  let+ (_, p) = indicated_proj_z(z);
  Projector.shape(p, syntax);
};

let id = (editor: Editor.t) => {
  switch (indicated_proj_ed(editor)) {
  | Some((id, _)) => id
  | None => Id.invalid
  };
};

let shape_from_map = (z, meta: Editor.Meta.t): option(shape) => {
  let* id = Indicated.index(z);
  let* syntax = Id.Map.find_opt(id, meta.projected.syntax_map);
  let ci = Id.Map.find_opt(id, meta.statics.info_map);
  let status = status(z);
  let info = {syntax, status, ci};
  shape(z, info);
};

let caret = (z: Zipper.t, meta: Editor.Meta.t): option(list(Node.t)) =>
  switch (shape_from_map(z, meta)) {
  | None => None
  | Some(Inline(_)) => None
  | Some(Block(_)) => Some([])
  };

let key_handler = (editor: Editor.t, key: Key.t): option(Action.project) =>
  switch (indicated_proj_ed(editor)) {
  | None => None
  | Some((id, p)) =>
    //let* syntax = Id.Map.find_opt(id, editor.state.meta.projected.syntax_map);
    let* (_, d, _) = Indicated.piece(editor.state.zipper);
    let (module P) = to_module(p);
    let+ action = P.keymap(d, key);
    handle(id, action);
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
    | Exp(Float)
    | Pat(Float) => [SliderF]
    | Exp(String)
    | Pat(String) => [TextArea]
    | _ => []
    }
  )
  @ [Projector.Fold]
  @ (
    switch (ci) {
    | InfoExp(_)
    | InfoPat(_) => [Info]
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
