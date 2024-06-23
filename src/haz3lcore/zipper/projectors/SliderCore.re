open ZipperBase;

let serialize = a =>
  a |> ZipperBase.sexp_of_slider_action |> Sexplib.Sexp.to_string;

let deserialize = a =>
  a |> Sexplib.Sexp.of_string |> ZipperBase.slider_action_of_sexp;

let of_mono = (syntax: Piece.t): option(string) =>
  switch (syntax) {
  | Tile({label: [l], _}) => Some(l)
  | _ => None
  };

let state_of = (piece: Piece.t): option(int) =>
  piece |> of_mono |> Option.map(int_of_string);

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
     let update = (action: string) =>
       switch (deserialize(action)) {
       | Set(value) => Slider({value: value})
       };
   });
