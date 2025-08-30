module Slider: Exo.Info = {
  let dev = "http://localhost:5173";
  let prod = WebEnv.base_url() ++ "/external/exoslider";

  let kind = ProjectorCore.Kind.ExoSlider;

  let init = (any: Language.Any.t): option(Exo.model) =>
    switch (any) {
    | Exp({term: Atom(Int(_)), _}) =>
      Some({
        width: 400,
        height: 160,
      })
    | _ => None
    };
};

module ValueBuilder: Exo.Info = {
  let dev = "http://localhost:5175";
  let prod = WebEnv.base_url() ++ "/external/exovaluebuilder";

  let kind = ProjectorCore.Kind.ExoValueBuilder;

  let init = (any: Language.Any.t): option(Exo.model) =>
    switch (HazelProtocol.JsonCodec.any_to_yojson(any)) {
    | Ok(_) =>
      /* Accept any expression that JsonCodec can handle */
      // TODO: More specific type restriction
      Some({
        width: 795,
        height: 200,
      })
    | Error(_) => None
    };
};

let module_of_kind = (kind: ProjectorCore.Kind.exo_kind): (module Exo.Info) =>
  switch (kind) {
  | ExoSlider => (module Slider)
  | ExoValueBuilder => (module ValueBuilder)
  };
