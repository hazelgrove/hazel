open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Projector;
open Util.OptUtil.Syntax;

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
      ~font_metrics,
      expected_ty,
      ~inject,
      ~measurement: Measured.measurement,
    ) =>
  div(
    ~attr=
      Attr.many([
        Attr.classes(["projector", "infer"] @ clss),
        Attr.on_pointerdown(_ =>
          Effect.Many([inject(Update.PerformAction(Project(Toggle(id))))])
        ),
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [Type.view(expected_ty), simple_shard(~font_metrics, ~measurement)],
  );

let display_ty = (expected_ty: option(Typ.t)): Typ.t =>
  switch (expected_ty) {
  | Some(expected_ty) => expected_ty
  | None => Var("-")
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
