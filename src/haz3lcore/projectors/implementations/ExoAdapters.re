open Util;

/* Slider-specific adapter for ExternalProjectorBridge */
module SliderAdapter: Exo.Info = {
  let exo_kind = ProjectorCore.Kind.ExoSlider;

  let hazel_origin = WebEnv.window_origin();

  let dev = "http://localhost:5173";

  let target_origin =
    WebEnv.choose_origin(
      ~name=ProjectorCore.Kind.exo_name(exo_kind),
      ~dev,
      ~prod=hazel_origin /* Use origin, not full path */
    );

  let codec_name = ProjectorCore.Kind.exo_name(exo_kind);

  let url = (id: Id.t) =>
    Printf.sprintf(
      "%s/?min=%d&max=%d&step=%d&id=%s&parentOrigin=%s",
      target_origin,
      0,
      100,
      1,
      Id.to_string(id),
      hazel_origin,
    );

  let term_to_string = (term: Language.Term.Any.t): option(string) =>
    switch (term) {
    | Exp({term: Atom(Int(i)), _}) => Some(Bigint.to_string(i))
    | _ => None
    };

  let string_to_term =
      (value_str: string, _: Language.Term.Any.t): Language.Term.Any.t =>
    Exp(
      Language.IdTagged.FreshGrammar.Exp.big_int(
        Bigint.of_string(value_str),
      ),
    );

  let init_test = (any: Language.Any.t): option(Exo.model) =>
    switch (term_to_string(any)) {
    | Some(_) =>
      Some({
        exo_kind,
        width: 400,
        height: 160,
      })
    | None => None
    };
};

/* ValueBuilder adapter for ExternalProjectorBridge */
module ValueBuilderAdapter: Exo.Info = {
  let exo_kind = ProjectorCore.Kind.ExoValueBuilder;

  let hazel_origin = WebEnv.window_origin();

  let dev = "http://localhost:5175";

  /* Target origin should be just the origin (scheme+host+port), not full URL
   * because iframe postMessage events always come from the origin, not the full path */
  let target_origin =
    WebEnv.choose_origin(
      ~name=ProjectorCore.Kind.exo_name(exo_kind),
      ~dev,
      ~prod=hazel_origin /* Use origin, not full path */
    );

  let codec_name = ProjectorCore.Kind.exo_name(exo_kind);

  let url = (id: Id.t) =>
    Printf.sprintf(
      "%s/?id=%s&parentOrigin=%s",
      target_origin,
      Id.to_string(id),
      hazel_origin,
    );

  /* Convert Hazel term to JSON string using JsonCodec */
  let term_to_string = (term: Language.Term.Any.t): option(string) =>
    switch (HazelProtocol.JsonCodec.any_to_yojson(term)) {
    | Ok(json) => Some(Yojson.Safe.to_string(json))
    | Error(_) => None
    };

  /* Convert JSON string to Hazel term using JsonCodec */
  let string_to_term =
      (value_str: string, _: Language.Term.Any.t): Language.Term.Any.t =>
    try({
      let json = Yojson.Safe.from_string(value_str);
      switch (HazelProtocol.JsonCodec.yojson_to_any(json)) {
      | Ok(term) => term
      | Error(_) =>
        /* Fallback to integer if JsonCodec fails */
        Exp(Language.IdTagged.FreshGrammar.Exp.big_int(Bigint.of_int(42)))
      };
    }) {
    | _ =>
      /* Fallback on parse error */
      Exp(Language.IdTagged.FreshGrammar.Exp.big_int(Bigint.of_int(42)))
    };

  /* Accept any expression that JsonCodec can handle */
  let init_test = (any: Language.Any.t): option(Exo.model) =>
    switch (HazelProtocol.JsonCodec.any_to_yojson(any)) {
    | Ok(_) =>
      Some({
        exo_kind,
        width: 795,
        height: 200 /* Start smaller, let content drive the size */
      })
    | Error(_) => None
    };
};
