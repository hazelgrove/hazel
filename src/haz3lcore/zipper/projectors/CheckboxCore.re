open Sexplib.Std;
open ZipperBase;

let of_mono = (syntax: Piece.t): option(string) =>
  switch (syntax) {
  | Tile({label: [l], _}) => Some(l)
  | _ => None
  };

let mk_mono = (sort: Sort.t, string: string): Piece.t =>
  string |> Form.mk_atomic(sort) |> Piece.mk_tile(_, []);

let state_of = (piece: Piece.t): option(bool) =>
  piece |> of_mono |> Option.map(bool_of_string);

let get = (piece: Piece.t): bool =>
  switch (piece |> of_mono |> Option.map(bool_of_string)) {
  | None => failwith("Checkbox: not boolean literal")
  | Some(s) => s
  };

let put = (bool: bool): Piece.t => bool |> string_of_bool |> mk_mono(Exp);

let toggle = (piece: Piece.t) => put(!get(piece));

let mk = (model): projector_core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = unit;
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = unit;
     let model = model;
     let projector = Checkbox(model);
     let can_project = p => state_of(p) != None;
     let placeholder_length = () => 2;
     let auto_update = _: projector => Checkbox();
     let update = _action => Checkbox();
   });
