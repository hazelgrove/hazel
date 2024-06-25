open Haz3lcore;
open Virtual_dom.Vdom;

let view = (~inject: Projector.action(_) => Ui_effect.t(unit), syntax, _) =>
  Node.input(
    ~attr=
      Attr.many(
        [
          Attr.create("type", "checkbox"),
          Attr.on_input((_, _) =>
            inject(UpdateSyntax(CheckboxCore.toggle))
          ),
          //JsUtil.stop_mousedown_propagation,
        ]
        @ (CheckboxCore.get(syntax) ? [Attr.checked] : []),
      ),
    [],
  );

let keymap = (_, key: Key.t): option(Projector.action(string)) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(Remove)
  | _ => None
  };

let mk =
    (syntax: Piece.t, _model: ZipperBase.checkbox, ~inject)
    : ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = ZipperBase.checkbox_action;

     let view = view(syntax, ~inject);
     let keymap = keymap;
   });
