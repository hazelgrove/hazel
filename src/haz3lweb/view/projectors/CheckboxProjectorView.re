open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

let put = (bool: bool) =>
  Example.mk_monotile(Form.mk_atomic(Exp, string_of_bool(bool)));

let get = piece =>
  switch (CheckboxProjectorCore.state_of(piece)) {
  | None => failwith("toggle: not a checkbox")
  | Some(s) => s
  };

let toggle = (piece: Piece.t) => {
  let cur =
    switch (CheckboxProjectorCore.state_of(piece)) {
    | None => failwith("toggle: not a checkbox")
    | Some(s) => s
    };
  put(!cur);
};

let wrap =
    (
      ~font_metrics,
      ~measurement: Measured.measurement,
      clss, // include projector name
      guy,
    ) =>
  div(
    ~attr=
      Attr.many([
        Attr.classes(["projector"] @ clss),
        JsUtil.stop_mousedown_propagation,
        DecUtil.abs_style(measurement, ~font_metrics),
      ]),
    [guy, PieceDec.convex_shard(~font_metrics, ~measurement)],
  );

let base =
    (~font_metrics, ~measurement: Measured.measurement, ~inject, clss, syntax) =>
  wrap(
    ~font_metrics,
    ~measurement,
    ["checkbox", ...clss],
    Node.input(
      ~attr=
        Attr.many(
          [
            Attr.create("type", "checkbox"),
            Attr.on_input((_evt, _str) =>
              inject(Projector.UpdateSyntax(toggle))
            ),
            JsUtil.stop_mousedown_propagation,
          ]
          @ (get(syntax) ? [Attr.checked] : []),
        ),
      [],
    ),
  );

let key_handler = (key: Key.t): option(Projector.action(unit)) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(Remove)
  | _ => None
  };

let mk =
    (syntax: Piece.t, ~inject, model: Projector.fold): ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = Projector.checkbox;
     type action = unit; //TODO(andrew)
     let model = model;
     let syntax = syntax;
     let inject = inject;
     let normal = base([], syntax, ~inject);
     let indicated = base(["indicated"], syntax, ~inject);
     let key_handler = key_handler;
     let ci_string = () => "F";
   });
