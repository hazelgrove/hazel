open Sexplib.Std;
open ZipperBase;

let state_of = (piece: Piece.t): option(bool) =>
  switch (piece) {
  | Tile({label: ["true"], _}) => Some(true)
  | Tile({label: ["false"], _}) => Some(false)
  | _ => None
  };

let get = (piece: Piece.t): bool =>
  switch (state_of(piece)) {
  | None => failwith("Checkbox: not boolean literal")
  | Some(s) => s
  };

let put = (bool: bool): Piece.t =>
  bool |> string_of_bool |> Form.mk_atomic(Exp) |> Piece.mk_tile(_, []);

let toggle = (piece: Piece.t) => put(!get(piece));

let mk = (model): projector_core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = unit;
     type action = unit;
     let model = model;
     let projector = Checkbox(model);
     let can_project = p => state_of(p) != None;
     let placeholder_length = () => 2;
     let auto_update = _: projector => Checkbox();
     let act = _ => Checkbox();
   });
