open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Sexplib.Std;

let view = (~inject) =>
  div(
    ~attr=Attr.on_double_click(_ => inject(Projector.Remove)),
    [text("⋱")],
  );

let key_handler = (key: Key.t): option(Projector.action(unit)) =>
  switch (key) {
  | {key: D("Escape"), _} => Some(Projector.Remove)
  | _ => None
  };

let mk =
    (syntax: Piece.t, model: Projector.fold, ~inject): ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = Projector.fold;
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = unit;
     let model = model;
     let syntax = syntax;
     let inject = inject;
     let view = view(~inject);
     let key_handler = key_handler;
   });
