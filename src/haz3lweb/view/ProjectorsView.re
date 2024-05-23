open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Projector;
open Util.OptUtil.Syntax;
open Sexplib.Std;

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
  //let proj_type: Projector.proj_type;
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
  let to_string: unit => string; //Projector //TODO: rename to ci_string or something
};

let mkFold = (data): (module PV) =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type t = unit;
     let data = data;
     let normal = fold_view([]);
     let indicated = fold_view(["indicated"]);
     let key_handler = (id, key: Key.t): option(UpdateAction.t) =>
       switch (key) {
       | {key: D("Escape"), _} =>
         Some(PerformAction(Project(Toggle(id))))
       | _ => None
       };
     let to_string = () => "F";
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
     let to_string: unit => string = _ => "I";
   });

let proj_view_m = (p: (module Projector.P)): (module PV) => {
  let (module P) = p;
  switch (P.proj_type) {
  | Fold(data) => mkFold(data^)
  | Infer(data) => mkInfer(data^)
  };
};

let key_handler =
    (p: (module Projector.P), id: Id.t, key: Key.t): option(UpdateAction.t) => {
  let (module PV) = proj_view_m(p);
  PV.key_handler(id, key);
};

let view =
    (id: Id.t, ps: Map.t, ~measured: Measured.t, ~inject, ~font_metrics) => {
  let* p = Projector.Map.find(id, ps);
  let+ measurement = Measured.find_by_id(id, measured);
  let (module PV) = proj_view_m(p);
  PV.normal(id, ~inject, ~font_metrics, ~measurement);
};

let indication_view =
    (id: Id.t, ps: Map.t, measured: Measured.t, ~inject, ~font_metrics)
    : option(Node.t) => {
  let* p = Projector.Map.find(id, ps);
  let+ measurement = Measured.find_by_id(id, measured);
  let (module PV) = proj_view_m(p);
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

let dispatch_key_to = (editor: Editor.t, key: Key.t): option(UpdateAction.t) =>
  switch (indicated_proj_ed(editor)) {
  | None => None
  | Some((id, p)) => key_handler(p, id, key)
  };

let ci = (~inject as _, editor: Editor.t) => {
  let+ (_, p) = indicated_proj_ed(editor);
  let (module PV) = proj_view_m(p);
  div(~attr=Attr.classes(["projector-ci"]), [text(PV.to_string())]);
};
