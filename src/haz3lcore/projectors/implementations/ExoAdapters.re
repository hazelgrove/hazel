open Util;

/* Slider-specific adapter for ExternalProjectorBridge */
module SliderAdapter = {
  let codec: ExternalProjectorBridge.codec = {
    syntax_to_json: (info: ProjectorBase.info) =>
      switch (info.utility.seg_to_term(info.syntax)) {
      | Some(Exp({term: Atom(Int(i)), _})) => Some(Bigint.to_string(i))
      | _ => None
      },
    json_to_segment: (info: ProjectorBase.info, value_str: string) =>
      info.utility.lift_syntax(
        fun
        | Exp(t) =>
          Exp({
            ...t,
            term: Atom(Int(Bigint.of_string(value_str))),
          })
        | _ => failwith("not an int literal"),
        info.syntax,
      ),
    codec_name: "int",
  };

  let target_origin = "http://localhost:5173";

  let build_url = (id: Id.t) =>
    Printf.sprintf(
      "%s/?min=%d&max=%d&step=%d&id=%s&parentOrigin=%s",
      target_origin,
      0,
      100,
      1,
      Id.to_string(id),
      "http://localhost:8000" /* Hazel dev server origin */
    );
};

[@deriving (show({with_path: false}), sexp, yojson)]
type exo_info = {
  codec: ExternalProjectorBridge.codec,
  target_origin: string,
  url: string,
};

let exo_info = (exo_kind: ProjectorCore.Kind.exo_kind, id: Id.t) =>
  switch (exo_kind) {
  | ExoSlider => {
      codec: SliderAdapter.codec,
      target_origin: SliderAdapter.target_origin,
      url: SliderAdapter.build_url(id),
    }
  };
