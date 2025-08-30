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

/* Send constraints to iframe */
let send_constraints = (id: Id.t, entry: Exo.entry): unit => {
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
let handle_ready = (id: Id.t, entry: Exo.entry): unit => {
  let init_msg =
    HazelProtocol.Init({
      id,
      value: entry.init_json,
    });
  post_from_hazel_message(init_msg, entry.target_origin, id);

  /* Send constraints after init */
  send_constraints(id, entry);
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

let handle_resize =
    (id: Id.t, width: int, height: int, entry: Exo.entry): unit => {
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
  | Resize({id, width, height}) => handle_resize(id, width, height, entry)
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
