open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Sexplib.Std;

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

     let view =
       div(
         ~attr=Attr.on_double_click(_ => inject(Projector.Remove)),
         [text("⋱")],
       );
     let key_handler = _: option(Projector.action(action)) => None;
   });
