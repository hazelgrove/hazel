open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Projector;

let fold_view = (~font_metrics, measurement) =>
  div(
    ~attr=
      Attr.many([
        Attr.classes(["fold"]),
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [text("⋱")],
  );

let infer = (~font_metrics, expected_ty, measurement) =>
  div(
    ~attr=
      Attr.many([
        Attr.classes(["infer"]),
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [Type.view(expected_ty)],
  );

let projector_view = (p: Projector.t, ~font_metrics, ~measurement) =>
  switch (p) {
  | Fold => Some(fold_view(~font_metrics, measurement))
  | Infer({expected_ty: None, _}) =>
    Some(infer(~font_metrics, Var("-"), measurement))
  | Infer({expected_ty: Some(expected_ty), _}) =>
    Some(infer(~font_metrics, expected_ty, measurement))
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
    (id, projectors, measured: Measured.t, ~font_metrics as _)
    : option(list(Node.t)) =>
  switch (get_proj_measure(id, projectors, measured: Measured.t)) {
  | Some((p, _measurement)) =>
    switch (p) {
    | Fold => Some([]) //TODO(andrew)
    | Infer(_) => Some([]) //TODO(andrew)
    }
  | None => None
  };
