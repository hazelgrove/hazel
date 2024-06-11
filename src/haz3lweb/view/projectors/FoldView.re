open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

let mk =
    (_syntax: Piece.t, model: ZipperBase.fold, ~inject): ProjectorViewModule.t =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = ZipperBase.fold;
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = ZipperBase.fold_action;

     let model = model;
     let view =
       div(
         ~attr=Attr.on_double_click(_ => inject(Projector.Remove)),
         [text("⋱")],
       );
     let keymap = _: option(Projector.action(action)) => None;
   });
