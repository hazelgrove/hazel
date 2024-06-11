open Haz3lcore;
open Virtual_dom.Vdom;

let view = (~inject: Projector.action(unit) => Ui_effect.t(unit), syntax) =>
  Node.input(
    ~attr=
      Attr.many(
        [
          Attr.create("type", "checkbox"),
          Attr.on_input((_, _) =>
            inject(UpdateSyntax(CheckboxCore.toggle))
          ),
          JsUtil.stop_mousedown_propagation,
        ]
        @ (CheckboxCore.get(syntax) ? [Attr.checked] : []),
      ),
    [],
  );

let keymap = (key: Key.t): option(Projector.action(unit)) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(Remove)
  | _ => None
  };

let mk =
    (syntax: Piece.t, ~inject, model: ZipperBase.checkbox)
    : ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = ZipperBase.checkbox;
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = ZipperBase.checkbox_action;

     let model = model;
     let view = view(syntax, ~inject);
     let keymap = keymap;
   });
