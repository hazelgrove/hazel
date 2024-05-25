open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

let remove = id => Update.PerformAction(Project(Remove(id)));

let base =
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
        JsUtil.stop_mousedown_propagation,
        Attr.on_double_click(_ => inject(remove(id))),
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [
      text(expected_ty |> InferProjectorCore.display_ty |> Typ.pretty_print),
      PieceDec.convex_shard(~font_metrics, ~measurement),
    ],
  );

let mk = (data: Projector.infer): ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type t = Projector.infer;
     let data = data;
     let normal = base([], data.expected_ty);
     let indicated = base(["indicated"], data.expected_ty);
     let key_handler = (id, key: Key.t) =>
       switch (key) {
       | {key: D("Escape"), _} => Some(remove(id))
       | _ => None
       };
     let ci_string: unit => string = _ => "I";
   });
