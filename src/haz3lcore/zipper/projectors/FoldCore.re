open Sexplib.Std;
open ZipperBase;

let mk = (): projector_core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = unit;
     type action = unit;
     let model = ();
     let projector = Fold();
     let can_project = _ => true;
     let placeholder_length = () => 2;
     let auto_update = _: projector => Fold();
     let act = _ => Fold();
   });
