open Sexplib.Std;
open ZipperBase;

let mk = (): projector_core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = unit;
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = unit;
     let model = ();
     let projector = Fold();
     let can_project = _ => true;
     let placeholder = () => Inline(2);
     let auto_update = _: projector => Fold();
     let update = _action => Fold();
   });
