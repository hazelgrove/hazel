open Haz3lcore;
open Virtual_dom.Vdom;
open Sexplib.Std;

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
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = unit;
     let model = model;
     let syntax = syntax;
     let inject = inject;
     let view = view(syntax, ~inject);
     let key_handler = key_handler;
   });
