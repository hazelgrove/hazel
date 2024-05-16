open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Projector;

let simple_shard = (~font_metrics, ~measurement: Measured.measurement) =>
  PieceDec.simple_shard(
    ~absolute=false,
    ~font_metrics,
    ~shapes=(Convex, Convex),
    ~path_cls=[],
    ~base_cls=[],
    measurement,
  );

let fold_view = (~font_metrics, measurement) =>
  div(
    ~attr=
      Attr.many([
        Attr.classes(["fold"]),
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [text("⋱"), simple_shard(~font_metrics, ~measurement)],
  );

let fold_indicated = (~font_metrics, ~measurement: Measured.measurement) =>
  div(
    ~attr=
      Attr.many([
        Attr.classes(["projector-indicated", "fold"]),
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [text("⋱"), simple_shard(~font_metrics, ~measurement)],
  );

let infer_view =
    (clss, ~font_metrics, expected_ty, ~measurement: Measured.measurement) =>
  div(
    ~attr=
      Attr.many([
        Attr.classes(clss),
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [Type.view(expected_ty), simple_shard(~font_metrics, ~measurement)],
  );

let projector_view = (p: Projector.t, ~font_metrics, ~measurement) =>
  switch (p) {
  | Fold => Some(fold_view(~font_metrics, measurement))
  | Infer({expected_ty: None, _}) =>
    Some(infer_view(["infer"], ~font_metrics, Var("-"), ~measurement))
  | Infer({expected_ty: Some(expected_ty), _}) =>
    Some(infer_view(["infer"], ~font_metrics, expected_ty, ~measurement))
  };

let get_proj_measure = (id, projectors, map) =>
  switch (Projector.Map.find(id, projectors)) {
  | Some(p) =>
    switch (Measured.find_by_id(id, map)) {
    | Some(measurement) => Some((p, measurement))
    | None => None
    }
  | None => None
  };

let view = (ps: Map.t, ~font_metrics, measured: Measured.t) =>
  List.filter_map(
    ((id, _p)) =>
      switch (get_proj_measure(id, ps, measured)) {
      | Some((p, measurement)) =>
        projector_view(p, ~font_metrics, ~measurement)
      | _ => None
      },
    Id.Map.bindings(ps),
  );

let indication_view =
    (id, projectors, measured: Measured.t, ~font_metrics)
    : option(list(Node.t)) =>
  switch (get_proj_measure(id, projectors, measured: Measured.t)) {
  | Some((p, measurement)) =>
    switch (p) {
    | Fold => Some([fold_indicated(~measurement, ~font_metrics)])
    | Infer({expected_ty, _}) =>
      let expected_ty =
        switch (expected_ty) {
        | Some(expected_ty) => expected_ty
        | None => Var("-")
        };
      Some([
        infer_view(
          ["projector-indicated", "infer"],
          ~measurement,
          expected_ty,
          ~font_metrics,
        ),
      ]);
    }
  | None => None
  };
