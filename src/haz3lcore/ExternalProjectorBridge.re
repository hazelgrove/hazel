open Util;
open ProjectorBase;
open Language;
open Js_of_ocaml;

/* Global registry to store projector entries by ID */
let registry: ref(Id.Map.t(Exo.entry)) = ref(Id.Map.empty);

/* Global effect scheduler - set by Main.re during initialization */
let global_effect_schedule: ref(option(Ui_effect.t(unit) => unit)) =
  ref(None);

let register = (entry: Exo.entry): unit => {
  print_endline("register url: " ++ entry.url);
  registry := Id.Map.add(entry.id, entry, registry^);
};

let iframe_id = (id: Id.t): string => Id.cls(id) ++ "-exo-iframe";

/* Send a typed message to iframe */
let post_from_hazel_message =
    (msg: HazelProtocol.from_hazel_message, target_origin: string, id: Id.t)
    : unit => {
  let js_msg = HazelProtocol.from_hazel_to_js(msg);
  JsUtil.post_to_iframe(iframe_id(id), target_origin, js_msg);
};

/* Message handlers using typed messages */
let handle_ready = (id: Id.t, entry: Exo.entry): unit => {
  let msg =
    HazelProtocol.Init({
      id,
      value: entry.init_json,
    });
  post_from_hazel_message(msg, entry.target_origin, id);
};

let handle_set_syntax = (value: string, entry: Exo.entry): unit =>
  switch (entry.json_to_segment(value)) {
  | Some(seg) =>
    switch (global_effect_schedule^) {
    | Some(scheduler) => scheduler(entry.signal(SetSyntax(seg)))
    | None => prerr_endline("setSyntax: no scheduler set")
    }
  | None => prerr_endline("setSyntax: codec conversion failed")
  };

let handle_resize = (width: int, height: int): unit => {
  /* TODO: Implement resize handling - might need to update projector model */
  prerr_endline(
    Printf.sprintf("handle_resize: %dx%d (not implemented)", width, height),
  );
};

let handle_request_focus = _: unit => {
  /* TODO: Implement focus handling */
  prerr_endline(
    "handle_request_focus: (not implemented)",
  );
};

let dispatch = (msg: HazelProtocol.to_hazel_message, entry: Exo.entry): unit =>
  switch (msg) {
  | Ready({id}) => handle_ready(id, entry)
  | SetSyntax({value, _}) => handle_set_syntax(value, entry)
  | Resize({width, height, _}) => handle_resize(width, height)
  | RequestFocus(_) => handle_request_focus(entry)
  };

let registry_lookup = (id: Id.t, origin: string): option(Exo.entry) =>
  switch (Id.Map.find_opt(id, registry^)) {
  | Some(entry) =>
    if (origin != entry.target_origin) {
      prerr_endline(
        "registry_lookup: origin mismatch: "
        ++ origin
        ++ " != "
        ++ entry.target_origin,
      );
      None;
    } else {
      Some(entry);
    }
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
