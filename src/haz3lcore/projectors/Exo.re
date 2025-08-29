open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type exo_model = {
  exo_kind: ProjectorCore.Kind.exo_kind,
  width: int,
  height: int,
};

module type Info = {
  let exo_kind: ProjectorCore.Kind.exo_kind;
  let codec_name: string;
  let term_to_string: Language.Term.Any.t => option(string);
  let string_to_term: (string, Language.Term.Any.t) => Language.Term.Any.t;
  let target_origin: string;
  let url: Id.t => string;
  let init_test: Language.Term.Any.t => option(exo_model);
};

/* Registry entry storing callback, info, codec, and target origin */
[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  id: Id.t,
  url: string,
  target_origin: string,
  codec_name: string,
  init_json: string,
  json_to_segment: string => option(Base.segment),
  signal: ProjectorBase.external_action => Ui_effect.t(unit),
};

let mk_entry =
    (
      signal: ProjectorBase.external_action => Ui_effect.t(unit),
      info: ProjectorBase.info,
      module Exo: Info,
    )
    : entry => {
  switch (
    switch (info.utility.seg_to_term(info.syntax)) {
    | Some(term) => Exo.term_to_string(term)
    | None => None
    }
  ) {
  | Some(init_json) => {
      signal,
      id: info.id,
      codec_name: Exo.codec_name,
      target_origin: Exo.target_origin,
      init_json,
      json_to_segment: (str: string) =>
        info.utility.lift_syntax(Exo.string_to_term(str), info.syntax),
      url: Exo.url(info.id),
    }
  | None => failwith("mk_entry: init syntax conversion failed")
  };
};
