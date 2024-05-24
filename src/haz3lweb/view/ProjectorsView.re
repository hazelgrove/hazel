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

let fold_view = (clss, id, ~font_metrics, ~inject, ~measurement) =>
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
    [text("⋱"), PieceDec.convex_shard(~font_metrics, ~measurement)],
  );

let infer_view =
    (
      clss,
      expected_ty: option(Typ.t),
      id: Id.t,
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
    [
      text(expected_ty |> display_ty |> Typ.pretty_print),
      //Type.view(expected_ty),
      PieceDec.convex_shard(~font_metrics, ~measurement),
    ],
  );

module type PV = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;
  let data: t;

  let normal:
    (
      Id.t,
      ~font_metrics: FontMetrics.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~measurement: Measured.measurement
    ) =>
    Node.t;

  let indicated:
    (
      Id.t,
      ~font_metrics: FontMetrics.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~measurement: Measured.measurement
    ) =>
    Node.t;

  let key_handler: (Id.t, Key.t) => option(UpdateAction.t);
  let ci_string: unit => string; //Projector //TODO: rename to ci_string or something
};

let mkFold = (data): (module PV) =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type t = Projector.fold;
     let data = data;
     let normal = fold_view([]);
     let indicated = fold_view(["indicated"]);
     let key_handler = (id, key: Key.t): option(UpdateAction.t) =>
       switch (key) {
       | {key: D("Escape"), _} =>
         Some(PerformAction(Project(Toggle(id))))
       | _ => None
       };
     let ci_string = () => "F";
   });

let mkInfer = (data): (module PV) =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type t = Projector.infer;
     let data = data;
     let normal = infer_view([], data.expected_ty);
     let indicated = infer_view(["indicated"], data.expected_ty);
     let key_handler = (id, key: Key.t): option(UpdateAction.t) =>
       switch (key) {
       | {key: D("Escape"), _} =>
         Some(PerformAction(Project(Toggle(id))))
       | _ => None
       };
     let ci_string: unit => string = _ => "I";
   });

let to_module = (p: Projector.t): (module PV) =>
  switch (p) {
  | Fold(data) => mkFold(data)
  | Infer(data) => mkInfer(data)
  };

let view =
    (id: Id.t, ps: Map.t, ~measured: Measured.t, ~inject, ~font_metrics) => {
  let* p = Projector.Map.find(id, ps);
  let+ measurement = Measured.find_by_id(id, measured);
  let (module PV) = to_module(p);
  PV.normal(id, ~inject, ~font_metrics, ~measurement);
};

let indication_view =
    (id: Id.t, ps: Map.t, measured: Measured.t, ~inject, ~font_metrics)
    : option(Node.t) => {
  let* p = Projector.Map.find(id, ps);
  let+ measurement = Measured.find_by_id(id, measured);
  let (module PV) = to_module(p);
  PV.indicated(id, ~inject, ~font_metrics, ~measurement);
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

let key_handler = (editor: Editor.t, key: Key.t): option(UpdateAction.t) =>
  switch (indicated_proj_ed(editor)) {
  | None => None
  | Some((id, p)) =>
    let (module PV) = to_module(p);
    PV.key_handler(id, key);
  };

let ci = (~inject as _, editor: Editor.t) => {
  let+ (_, p) = indicated_proj_ed(editor);
  let (module PV) = to_module(p);
  div(~attr=Attr.classes(["projector-ci"]), [text(PV.ci_string())]);
};
