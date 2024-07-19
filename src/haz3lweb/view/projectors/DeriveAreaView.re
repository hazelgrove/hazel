open Haz3lcore;
open Virtual_dom.Vdom;

let view = (~inject: Projector.action(_) => Ui_effect.t(unit), syntax, _) => {
  ignore(inject);
  ignore(syntax);
  Node.div([]);
};

let keymap = (_, key: Key.t): option(Projector.action(string)) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(Remove)
  | _ => None
  };

let mk =
    (syntax: Piece.t, _model: ZipperBase.derivearea, ~inject)
    : ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = ZipperBase.derivearea_action;
     let view = view(syntax, ~inject);
     let keymap = keymap;
   });
