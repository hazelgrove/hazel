open ZipperBase;

let of_mono = (syntax: Piece.t): option(string) =>
  switch (syntax) {
  | Tile({label: [l], _}) => Some(l)
  | _ => None
  };

let mk_mono = (sort: Sort.t, string: string): Piece.t =>
  string |> Form.mk_atomic(sort) |> Piece.mk_tile(_, []);

let state_of = (piece: Piece.t): option(int) =>
  piece |> of_mono |> Option.map(int_of_string);

let put = (new_val: string): Piece.t => mk_mono(Exp, new_val);

let get = (piece: Piece.t): int =>
  switch (piece |> of_mono |> Option.map(int_of_string)) {
  | None => failwith("Slider: not integer literal")
  | Some(s) => s
  };

let mk = (model): projector_core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = ZipperBase.slider;
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = ZipperBase.slider_action;
     let model = model;
     let projector = Slider(model);
     let can_project = p => state_of(p) != None;
     let placeholder = () => Inline(10);
     let auto_update = _: projector => Slider(model);
     let update = _ => Slider(model);
   });
