open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type model = {
  //exo_kind: ProjectorCore.Kind.exo_kind,
  width: int,
  height: int,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | Resize(int, int);

module type Info = {
  let kind: ProjectorCore.Kind.exo_kind;
  let dev: string;
  let prod: string;
  let init: Language.Term.Any.t => option(model);
};

/* Registry entry storing callback, info, codec, and target origin */
[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  id: Id.t,
  url: string,
  target_origin: string,
  init_json: string,
  json_to_segment: string => option(Base.segment),
  [@sexp.opaque] [@yojson.opaque]
  signal: ProjectorBase.external_action => Ui_effect.t(unit),
  [@sexp.opaque] [@yojson.opaque]
  inject: action => Ui_effect.t(unit),
};

/* Convert Hazel term to JSON string using JsonCodec */
let term_to_string = (term: Language.Term.Any.t): option(string) =>
  switch (HazelProtocol.JsonCodec.any_to_yojson(term)) {
  | Ok(json) => Some(Yojson.Safe.to_string(json))
  | Error(_) => None
  };

/* Convert JSON string to Hazel term using JsonCodec */
let string_to_term =
    (value_str: string, exp: Language.Term.Any.t): Language.Term.Any.t =>
  try({
    let json = Yojson.Safe.from_string(value_str);
    switch (HazelProtocol.JsonCodec.yojson_to_any(json)) {
    | Ok(term) => term
    | Error(msg) =>
      print_endline("JsonCodec failed : " ++ msg ++ ", using existing term");
      exp;
    };
  }) {
  | _ =>
    print_endline("JsonCodec failed, using existing term");
    exp;
  };

let mk_entry =
    (
      signal: ProjectorBase.external_action => Ui_effect.t(unit),
      inject: action => Ui_effect.t(unit),
      info: ProjectorBase.info,
      module Exo: Info,
    )
    : entry => {
  switch (
    switch (info.utility.seg_to_term(info.syntax)) {
    | Some(term) => term_to_string(term)
    | None => None
    }
  ) {
  | Some(init_json) =>
    let target_origin =
      WebEnv.choose_origin(
        ~name=ProjectorCore.Kind.exo_name(Exo.kind),
        ~dev=Exo.dev,
        ~prod=Exo.prod,
      );
    {
      signal,
      inject,
      id: info.id,
      // codec_name: ProjectorCore.Kind.exo_name(Exo.exo_kind),
      target_origin,
      init_json,
      json_to_segment: (str: string) =>
        info.utility.lift_syntax(string_to_term(str), info.syntax),
      url:
        Printf.sprintf(
          "%sid=%s&parentOrigin=%s",
          target_origin,
          Id.to_string(info.id),
          WebEnv.window_origin(),
        ),
    };
  | None => failwith("mk_entry: init syntax conversion failed")
  };
};
