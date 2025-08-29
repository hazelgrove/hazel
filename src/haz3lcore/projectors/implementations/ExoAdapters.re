open Util;

let hazel_server = "http://localhost:8000"; /* Hazel dev server origin */

/* Slider-specific adapter for ExternalProjectorBridge */
module SliderAdapter: Exo.Info = {
  let exo_kind = ProjectorCore.Kind.ExoSlider;

  let target_origin = "http://localhost:5173";

  let codec_name = ProjectorCore.Kind.exo_name(exo_kind);

  let url = (id: Id.t) =>
    Printf.sprintf(
      "%s/?min=%d&max=%d&step=%d&id=%s&parentOrigin=%s",
      target_origin,
      0,
      100,
      1,
      Id.to_string(id),
      hazel_server,
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

  let init_test = (any: Language.Any.t): bool =>
    switch (term_to_string(any)) {
    | Some(_) => true
    | None => false
    };
};
