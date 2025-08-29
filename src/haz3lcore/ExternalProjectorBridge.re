open Util;
open ProjectorBase;
open Language;
open Js_of_ocaml;

/* Codec interface for converting between Hazel syntax and external app JSON */
[@deriving (show({with_path: false}), sexp, yojson)]
type codec = {
  syntax_to_json: ProjectorBase.info => option(string),
  json_to_segment: (ProjectorBase.info, string) => option(Base.segment),
  codec_name: string,
};

/* Registry entry storing callback, info, codec, and target origin */
[@deriving (show({with_path: false}), sexp, yojson)]
type projector_entry = {
  signal: ProjectorBase.external_action => Ui_effect.t(unit),
  info: ProjectorBase.info,
  target_origin: string,
  codec,
};

type message = {
  t: string,
  id: Id.t,
  value: string,
  codec: string,
};

/* Global registry to store projector entries by ID */
let registry: ref(Id.Map.t(projector_entry)) = ref(Id.Map.empty);

/* Global effect scheduler - set by Main.re during initialization */
let global_effect_schedule: ref(option(Ui_effect.t(unit) => unit)) =
  ref(None);

let register =
    (
      codec: codec,
      target_origin: string,
      signal: ProjectorBase.external_action => Ui_effect.t(unit),
      info: ProjectorBase.info,
    )
    : unit => {
  let entry = {
    signal,
    info,
    codec,
    target_origin,
  };
  registry := Id.Map.add(info.id, entry, registry^);
};

let unregister_projector = (id: Id.t): unit =>
  registry := Id.Map.remove(id, registry^);

let iframe_id = (id: Id.t): string => Id.cls(id) ++ "-exo-iframe";

let set_msg_attr = (msg, attr, value) =>
  Js.Unsafe.set(msg, attr, Js.string(value));

let mk_msg = (message: message) => {
  let msg = Js.Unsafe.obj([||]);
  set_msg_attr(msg, "type", message.t);
  set_msg_attr(msg, "id", Id.to_string(message.id));
  set_msg_attr(msg, "codec", message.codec);
  set_msg_attr(msg, "value", message.value);
  msg;
};

let post_msg = (msg, target_origin, id: Id.t) =>
  msg |> mk_msg |> JsUtil.post_to_iframe(iframe_id(id), target_origin);

let handle_ready = (msg: message, entry: projector_entry): unit =>
  switch (entry.codec.syntax_to_json(entry.info)) {
  | Some(value) =>
    let msg = {
      t: "init",
      id: msg.id,
      value,
      codec: entry.codec.codec_name,
    };
    post_msg(msg, entry.target_origin, msg.id);
  | None => prerr_endline("ready: codec conversion failed")
  };

let handle_set_syntax = (msg: message, entry: projector_entry): unit =>
  switch (entry.codec.json_to_segment(entry.info, msg.value)) {
  | Some(seg) =>
    switch (global_effect_schedule^) {
    | Some(scheduler) => scheduler(entry.signal(SetSyntax(seg)))
    | None => prerr_endline("setSyntax: no scheduler set")
    }
  | None => prerr_endline("setSyntax: codec conversion failed")
  };

let dispatch = (msg: message, entry: projector_entry): unit =>
  switch (msg.t) {
  | "setSyntax" => handle_set_syntax(msg, entry)
  | "ready" => handle_ready(msg, entry)
  | other => prerr_endline("dispatch: unknown message type: " ++ other)
  };

let registry_lookup = (msg: message, origin: string): option(projector_entry) =>
  switch (Id.Map.find_opt(msg.id, registry^)) {
  | Some(entry) =>
    if (origin != entry.target_origin) {
      prerr_endline(
        "registry_lookup: origin mismatch: "
        ++ origin
        ++ " != "
        ++ entry.target_origin,
      );
      None;
    } else if (msg.codec != entry.codec.codec_name) {
      prerr_endline(
        "registry_lookup: codec mismatch: "
        ++ msg.codec
        ++ " != "
        ++ entry.codec.codec_name,
      );
      None;
    } else {
      Some(entry);
    }
  | None =>
    prerr_endline("listener: projector not found");
    None;
  };

let get_msg_attr = (obj: Js.t(_), key: string): string =>
  try(Js.to_string(Js.Unsafe.get(obj, key))) {
  | _ => "unknown"
  };

let parse_message = (data): option(message) =>
  switch (Id.of_string(get_msg_attr(data, "id"))) {
  | Some(id) =>
    Some({
      t: get_msg_attr(data, "type"),
      id,
      value: get_msg_attr(data, "value"),
      codec: get_msg_attr(data, "codec"),
    })
  | exception _ =>
    prerr_endline("parse_message: No UUID found");
    None;
  | None => None
  };

let listener = (event: _) => {
  switch (parse_message(event##.data)) {
  | Some(msg) =>
    switch (registry_lookup(msg, Js.to_string(event##.origin))) {
    | Some(entry) => dispatch(msg, entry)
    | None => ()
    }
  | None => prerr_endline("listener: invalid message format")
  };
  Js._true;
};

let init = (scheduler: Ui_effect.t(unit) => unit): unit => {
  global_effect_schedule := Some(scheduler);
  JsUtil.add_message_listener(listener);
};
