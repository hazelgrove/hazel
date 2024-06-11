open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

let remove = id => Update.PerformAction(Project(Remove(id)));

let base =
    (
      clss,
      id: Id.t,
      expected_ty: option(Typ.t),
      ~font_metrics,
      ~inject,
      ~measurement: Measured.measurement,
    ) =>
  div(
    ~attr=
      Attr.many([
        Attr.classes(["projector", "infer"] @ clss),
        JsUtil.stop_mousedown_propagation,
        Attr.on_double_click(_ => inject(remove(id))),
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [
      text(expected_ty |> InferProjectorCore.display_ty |> Typ.pretty_print),
      PieceDec.convex_shard(~font_metrics, ~measurement),
    ],
  );

let key_handler = (id: Id.t, key: Key.t): option(UpdateAction.t) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(remove(id))
  | _ => None
  };

let mk =
    (id: Id.t, syntax: Piece.t, model: Projector.infer): ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = Projector.infer;
     let model = model;
     let id = id;
     let syntax = syntax;
     let normal = base([], id, model.expected_ty);
     let indicated = base(["indicated"], id, model.expected_ty);
     let key_handler = key_handler(id);
     let ci_string: unit => string = _ => "I";
   });
