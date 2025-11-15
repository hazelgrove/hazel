open Util;
open ProjectorBase;
open Language;
open Js_of_ocaml;

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
  inject: Exo.action => Ui_effect.t(unit),
};

/* Convert Hazel term to JSON string using JsonCodec */
let term_to_string = (term: Language.Any.t): option(string) =>
  switch (HazelProtocol.JsonCodec.any_to_yojson(term)) {
  | Ok(json) => Some(Yojson.Safe.to_string(json))
  | Error(err) =>
    prerr_endline("term_to_string: " ++ err);
    None;
  };

/* Convert JSON string to Hazel term using JsonCodec */
let string_to_term = (value_str: string, exp: Language.Any.t): Language.Any.t =>
  try({
    let json = Yojson.Safe.from_string(value_str);
    switch (HazelProtocol.JsonCodec.yojson_to_any(json)) {
    | Ok(term) => term
    | Error(msg) =>
      prerr_endline("JsonCodec failed : " ++ msg ++ ", using existing term");
      exp;
    };
  }) {
  | _ =>
    prerr_endline("JsonCodec failed, using existing term");
    exp;
  };

let mk_entry =
    (
      signal: ProjectorBase.external_action => Ui_effect.t(unit),
      inject: Exo.action => Ui_effect.t(unit),
      info: ProjectorBase.info,
      exo: Exo.info,
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
        ~name=Exo.name(exo.kind),
        ~dev=exo.dev,
        ~prod=exo.prod,
      );
    {
      signal,
      inject,
      id: info.id,
      target_origin,
      init_json,
      json_to_segment: (str: string) =>
        info.utility.lift_syntax(
          string_to_term(str),
          Inline.Zzt,
          info.syntax,
        ),
      url:
        Printf.sprintf(
          "%s/?id=%s&parentOrigin=%s",
          target_origin,
          Id.to_string(info.id),
          WebEnv.window_origin(),
        ),
    };
  | None => failwith("mk_entry: init syntax conversion failed")
  };
};

/* Global registry to store projector entries by ID */
let registry: ref(Id.Map.t(entry)) = ref(Id.Map.empty);

/* Global effect scheduler - set by Main.re during initialization */
let global_effect_schedule: ref(option(Ui_effect.t(unit) => unit)) =
  ref(None);

let register = (parent, local, info, exo: Exo.info): string => {
  let entry = mk_entry(parent, local, info, exo);
  print_endline("register url: " ++ entry.url);
  print_endline("TODO: Don't do this every draw");
  registry := Id.Map.add(entry.id, entry, registry^);
  entry.url;
};

let iframe_id = (id: Id.t): string => Id.cls(id) ++ "-exo-iframe";

/* Send a typed message to iframe */
let post_from_hazel_message =
    (msg: HazelProtocol.from_hazel_message, target_origin: string, id: Id.t)
    : unit => {
  let js_msg = HazelProtocol.from_hazel_to_js(msg);
  JsUtil.post_to_iframe(iframe_id(id), target_origin, js_msg);
};

/* Send constraints to iframe */
let send_constraints = (id: Id.t, entry: entry): unit => {
  /* TODO: Compute these from editor layout and adapter config */
  let max_width = 800;
  let max_height = 600;
  let min_width = Some(200);
  let min_height = Some(100);
  let msg =
    HazelProtocol.(
      Constraints({
        id,
        max_width,
        max_height,
        min_width,
        min_height,
      })
    );
  post_from_hazel_message(msg, entry.target_origin, id);
};

/* Message handlers using typed messages */
let handle_ready = (id: Id.t, entry: entry): unit => {
  let init_msg =
    HazelProtocol.Init({
      id,
      value: entry.init_json,
    });
  post_from_hazel_message(init_msg, entry.target_origin, id);
  //TODO(andrew): consider reinstating
  //send_constraints(id, entry);
};

let handle_set_syntax = (value: string, entry: entry): unit =>
  switch (entry.json_to_segment(value)) {
  | Some(seg) =>
    switch (global_effect_schedule^) {
    | Some(scheduler) => scheduler(entry.signal(SetSyntax(seg)))
    | None => prerr_endline("setSyntax: no scheduler set")
    }
  | None => prerr_endline("setSyntax: codec conversion failed")
  };

let handle_resize = (id: Id.t, width: int, height: int, entry: entry): unit => {
  Printf.printf(
    "📏 Received resize: %s %dx%d\n",
    Id.to_string(id),
    width,
    height,
  );

  /* Apply constraints */
  let max_width = 800; /* TODO: compute from editor layout */
  let max_height = 600; /* TODO: configurable per adapter */
  let min_width = 200;
  let min_height = 100;

  let constrained_width = max(min_width, min(max_width, width));
  let constrained_height = max(min_height, min(max_height, height));

  /* Log resize for debugging */
  if (abs(width - constrained_width) >= 2
      || abs(height - constrained_height) >= 2) {
    Printf.printf(
      "Resize %s: %dx%d -> %dx%d (constrained)\n",
      Id.to_string(id),
      width,
      height,
      constrained_width,
      constrained_height,
    );
  } else {
    Printf.printf(
      "Resize %s: %dx%d (within constraints)\n",
      Id.to_string(id),
      width,
      height,
    );
  };

  /* Trigger projector update through MVU loop */
  switch (global_effect_schedule^) {
  | Some(scheduler) =>
    scheduler(entry.inject(Resize(constrained_width, constrained_height)))
  | None => prerr_endline("resize: no scheduler set")
  };

  /* Send updated constraints if size was clamped */
  if (constrained_width != width || constrained_height != height) {
    send_constraints(id, entry);
  };
};

let dispatch = (msg: HazelProtocol.to_hazel_message, entry: entry): unit =>
  switch (msg) {
  | Ready({id}) => handle_ready(id, entry)
  | SetSyntax({value, _}) => handle_set_syntax(value, entry)
  | Resize({id, width, height}) => handle_resize(id, width, height, entry)
  };

/* Extract the origin (scheme://host:port) from a URL */
let extract_origin = (url: string): string =>
  try({
    let protocol_end = String.index(url, ':');
    let protocol = String.sub(url, 0, protocol_end);
    if (String.length(url) > protocol_end
        + 2
        && String.equal(String.sub(url, protocol_end, 3), "://")) {
      let after_protocol = protocol_end + 3;
      let remaining =
        String.sub(url, after_protocol, String.length(url) - after_protocol);
      let host_end =
        try(String.index(remaining, '/')) {
        | Not_found => String.length(remaining)
        };
      let host_part = String.sub(remaining, 0, host_end);
      protocol ++ "://" ++ host_part;
    } else {
      url;
    };
  }) {
  | _ => url
  };

let registry_lookup = (id: Id.t, origin: string): option(entry) =>
  switch (Id.Map.find_opt(id, registry^)) {
  | Some(entry) =>
    let expected_origin = extract_origin(entry.target_origin);
    if (origin != expected_origin) {
      prerr_endline(
        "registry_lookup: origin mismatch: "
        ++ origin
        ++ " != "
        ++ expected_origin
        ++ " (from target_origin: "
        ++ entry.target_origin
        ++ ")",
      );
      None;
    } else {
      Some(entry);
    };
  | None => None
  };

let listener = (event: _) => {
  switch (HazelProtocol.parse_to_hazel_message(event##.data)) {
  | Some(msg) =>
    switch (
      registry_lookup(
        HazelProtocol.id_of(msg),
        Js.to_string(event##.origin),
      )
    ) {
    | Some(entry) => dispatch(msg, entry)
    | None => prerr_endline("listener: projector not found")
    }
  | None => prerr_endline("listener: invalid message format")
  };
  Js._true;
};

let init = (scheduler: Ui_effect.t(unit) => unit): unit => {
  global_effect_schedule := Some(scheduler);
  JsUtil.add_message_listener(listener);
};
