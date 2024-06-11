open Sexplib.Std;
open ZipperBase;

let mk = (model): projector_core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = unit;
     type action = unit;
     let model = model;
     let projector = Fold(model);
     let can_project = Piece.is_convex;
     let placeholder_length = () => 2;
     let auto_update = _: projector => Fold();
     let act = _ => Fold();
   });
