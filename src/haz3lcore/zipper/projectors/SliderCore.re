open ZipperBase;

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
     let placeholder_length = () => 10;
     let auto_update = _: projector => Slider(model);
     let update =
       fun
       | Set(value) => Slider({value: value});
   });
