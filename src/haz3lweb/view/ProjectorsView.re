open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Projector;
open Util.OptUtil.Syntax;

let stop_mousedown_propagation =
  Attr.on_mousedown(evt => {
    Js_of_ocaml.Dom_html.stopPropagation(evt);
    Virtual_dom.Vdom.Effect.Ignore;
  });

let simple_shard = (~font_metrics, ~measurement: Measured.measurement) =>
  PieceDec.simple_shard(
    ~absolute=false,
    ~font_metrics,
    ~shapes=(Convex, Convex),
    ~path_cls=[],
    ~base_cls=[],
    measurement,
  );

let fold_view = (id, clss, ~font_metrics, ~inject, ~measurement) =>
  div(
    ~attr=
      Attr.many([
        Attr.classes(["projector", "fold"] @ clss),
        stop_mousedown_propagation,
        Attr.on_pointerdown(_ =>
          inject(Update.PerformAction(Project(Toggle(id))))
        ),
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [text("⋱"), simple_shard(~font_metrics, ~measurement)],
  );

let infer_view =
    (
      id: Id.t,
      clss,
      expected_ty,
      ~font_metrics,
      ~inject,
      ~measurement: Measured.measurement,
    ) =>
  div(
    ~attr=
      Attr.many([
        Attr.classes(["projector", "infer"] @ clss),
        stop_mousedown_propagation,
        Attr.on_pointerdown(_ =>
          Effect.Many([inject(Update.PerformAction(Project(Toggle(id))))])
        ),
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [Type.view(expected_ty), simple_shard(~font_metrics, ~measurement)],
  );

let display_ty = (expected_ty: option(Typ.t)): Typ.t =>
  switch (expected_ty) {
  | Some(expected_ty) =>
    print_endline(
      "ProjectorsView: infer_view. expected_ty:" ++ Typ.show(expected_ty),
    );
    expected_ty;
  | None =>
    print_endline("ProjectorsView: infer_view. expected_ty: None");
    Var("-");
  };

let projector_view =
    (id, p: Projector.t, ~inject, ~font_metrics, ~measurement) =>
  switch (p) {
  | Fold => fold_view(id, [], ~inject, ~font_metrics, ~measurement)
  | Infer({expected_ty, _}) =>
    let ty = display_ty(expected_ty);
    infer_view(id, [], ~inject, ~font_metrics, ty, ~measurement);
  };

let indicated_view =
    (id, p: Projector.t, ~inject, ~font_metrics, ~measurement) =>
  switch (p) {
  | Fold =>
    fold_view(id, ["indicated"], ~measurement, ~font_metrics, ~inject)
  | Infer({expected_ty, _}) =>
    let ty = display_ty(expected_ty);
    infer_view(id, ["indicated"], ~inject, ty, ~measurement, ~font_metrics);
  };

let key_handler = (p: t, id: Id.t, key: Key.t): option(UpdateAction.t) =>
  switch (p) {
  | Infer(_) =>
    switch (key) {
    | {key: D("Escape"), _} => Some(PerformAction(Project(Toggle(id))))
    | _ => None
    }
  | Fold =>
    switch (key) {
    | {key: D("Escape"), _} => Some(PerformAction(Project(Toggle(id))))
    | _ => None
    }
  };

let view =
    (id: Id.t, ps: Map.t, ~measured: Measured.t, ~inject, ~font_metrics) => {
  let* p = Projector.Map.find(id, ps);
  let+ measurement = Measured.find_by_id(id, measured);
  projector_view(id, p, ~inject, ~font_metrics, ~measurement);
};

let indication_view =
    (id: Id.t, ps: Map.t, measured: Measured.t, ~inject, ~font_metrics)
    : option(Node.t) => {
  let* p = Projector.Map.find(id, ps);
  let+ measurement = Measured.find_by_id(id, measured);
  indicated_view(id, p, ~inject, ~font_metrics, ~measurement);
};

let view_all = (ps: Map.t, measured: Measured.t, ~inject, ~font_metrics) =>
  List.filter_map(
    ((id, _)) => view(id, ps, ~measured, ~inject, ~font_metrics),
    Id.Map.bindings(ps),
  );

let indicated_proj_ed = (editor: Editor.t) => {
  let projectors = Editor.get_projectors(editor);
  //TODO: use z_proj instead of zipper?
  let* id = Indicated.index(editor.state.zipper);
  let+ projector = Projector.Map.find(id, projectors);
  (id, projector);
};

let dispatch_key_to = (editor: Editor.t, key: Key.t): option(UpdateAction.t) =>
  switch (indicated_proj_ed(editor)) {
  | None => None
  | Some((id, p)) => key_handler(p, id, key)
  };

let ci = (~inject as _, editor: Editor.t) => {
  let+ (_, projector) = indicated_proj_ed(editor);
  div(
    ~attr=Attr.classes(["projector-ci"]),
    [text(Projector.to_string(projector))],
  );
};
