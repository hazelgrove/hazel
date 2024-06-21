open ZipperBase;

let serialize = a =>
  a |> ZipperBase.sexp_of_slider_action |> Sexplib.Sexp.to_string;

let deserialize = a =>
  a |> Sexplib.Sexp.of_string |> ZipperBase.slider_action_of_sexp;

let mk = (model): projector_core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = ZipperBase.slider;
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = ZipperBase.slider_action;
     let model = model;
     let projector = Slider(model);
     let can_project = _ => true;
     let placeholder = () => Inline(10);
     let auto_update = _: projector => Slider(model);
     let update = (action: string) =>
       switch (deserialize(action)) {
       | Set(value) => Slider({value: value})
       };
   });
