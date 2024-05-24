open Sexplib.Std;
open ZipperBase;

let mk = (data): projector_module =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type t = unit;
     let data = data;
     let proj_type = Fold(data);
     let can_project = Piece.is_convex;
     let placeholder_length = () => 2;
     let update = _: proj_type => Fold();
   });
