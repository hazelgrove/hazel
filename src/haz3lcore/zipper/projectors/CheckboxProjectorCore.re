open Sexplib.Std;
open ZipperBase;

let state_of = (piece: Piece.t): option(bool) =>
  switch (piece) {
  | Tile({label: ["true"], _}) => Some(true)
  | Tile({label: ["false"], _}) => Some(false)
  | _ => None
  };

let mk = (model): projector_core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = unit;
     type action = unit;
     let model = model;
     let projector = Checkbox(model);
     let can_project = p => Piece.is_convex(p) && state_of(p) != None;
     let placeholder_length = () => 2;
     let auto_update = _: projector => Checkbox();
     let act = _ => Checkbox();
   });
