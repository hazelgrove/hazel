open Util;
open ProjectorBase;
open Language;

/* Codec interface for converting between Hazel syntax and external app JSON */
[@deriving (show({with_path: false}), sexp, yojson)]
type codec = {
  syntax_to_string: ProjectorBase.info => option(string),
  json_to_segment: (ProjectorBase.info, string) => option(Base.segment),
  codec_name: string,
};

/* Registry entry storing callback, info, codec, and target origin */
[@deriving (show({with_path: false}), sexp, yojson)]
type projector_entry = {
  parent_callback: ProjectorBase.external_action => Ui_effect.t(unit),
  info: ProjectorBase.info,
  codec,
  target_origin: string,
};

/* Global registry to store projector entries by ID */
let projector_registry: ref(Id.Map.t(projector_entry)) = ref(Id.Map.empty);

/* Global effect scheduler - set by Main.re during initialization */
let global_effect_schedule: ref(option(Ui_effect.t(unit) => unit)) =
  ref(None);

let set_effect_scheduler = (scheduler: Ui_effect.t(unit) => unit): unit => {
  global_effect_schedule := Some(scheduler);
};

let register_projector =
    (
      codec: codec,
      target_origin: string,
      parent_callback: ProjectorBase.external_action => Ui_effect.t(unit),
      info: ProjectorBase.info,
    )
    : unit => {
  let entry = {
    parent_callback,
    info,
    codec,
    target_origin,
  };
  projector_registry := Id.Map.add(info.id, entry, projector_registry^);
};

let unregister_projector = (id: Id.t): unit => {
  projector_registry := Id.Map.remove(id, projector_registry^);
};

/* Helper to send postMessage to iframe contentWindow */
let post_to_iframe =
    (id: Id.t, target_origin: string, message_obj: Js_of_ocaml.Js.t('a))
    : unit => {
  let iframe_id = Id.cls(id) ++ "-exo-iframe";
  let doc = Js_of_ocaml.Dom_html.document;
  switch (
    Js_of_ocaml.Js.Opt.to_option(
      doc##getElementById(Js_of_ocaml.Js.string(iframe_id)),
    )
  ) {
  | Some(iframe_element) =>
    let iframe = Js_of_ocaml.Js.Unsafe.coerce(iframe_element);
    switch (Js_of_ocaml.Js.Opt.to_option(iframe##.contentWindow)) {
    | Some(content_window) =>
      content_window##postMessage(
        message_obj,
        Js_of_ocaml.Js.string(target_origin),
      )
    | None => () /* contentWindow not available */
    };
  | None => () /* iframe not found */
  };
};

/* Full postMessage handler with codec support */
let init = (): unit => {
  let handle_message = event => {
    let origin = Js_of_ocaml.Js.to_string(event##.origin);

    try({
      let data = Js_of_ocaml.Js.Unsafe.inject(event##.data);
      let get_string = (obj, key) =>
        try({
          let value = Js_of_ocaml.Js.Unsafe.get(obj, key);
          Js_of_ocaml.Js.to_string(value);
        }) {
        | _ => "unknown"
        };

      let msg_type = get_string(data, "type");
      let msg_id = get_string(data, "id");
      let msg_value = get_string(data, "value");
      let msg_codec = get_string(data, "codec");

      /* Handle setSyntax messages */
      if (msg_type == "setSyntax") {
        switch (Id.of_string(msg_id)) {
        | Some(projector_id) =>
          switch (Id.Map.find_opt(projector_id, projector_registry^)) {
          | Some({parent_callback, info, codec, target_origin}) =>
            /* Verify origin matches this projector's target */
            if (origin == target_origin && msg_codec == codec.codec_name) {
              switch (codec.json_to_segment(info, msg_value)) {
              | Some(new_segment) =>
                let effect = parent_callback(SetSyntax(new_segment));
                /* Schedule the effect for execution */
                switch (global_effect_schedule^) {
                | Some(scheduler) => scheduler(effect)
                | None => () /* No scheduler set */
                };
              | None => () /* Codec conversion failed */
              };
            }
          | None => () /* Projector not found */
          }
        | None => () /* Invalid UUID format */
        };
      };

      /* Handle ready messages - send init with current value */
      if (msg_type == "ready") {
        switch (Id.of_string(msg_id)) {
        | Some(projector_id) =>
          switch (Id.Map.find_opt(projector_id, projector_registry^)) {
          | Some({info, codec, target_origin, _}) =>
            /* Verify origin matches this projector's target */
            if (origin == target_origin) {
              switch (codec.syntax_to_string(info)) {
              | Some(current_value) =>
                let init_message = Js_of_ocaml.Js.Unsafe.obj([||]);
                Js_of_ocaml.Js.Unsafe.set(
                  init_message,
                  "type",
                  Js_of_ocaml.Js.string("init"),
                );
                Js_of_ocaml.Js.Unsafe.set(
                  init_message,
                  "id",
                  Js_of_ocaml.Js.string(msg_id),
                );
                Js_of_ocaml.Js.Unsafe.set(
                  init_message,
                  "codec",
                  Js_of_ocaml.Js.string(codec.codec_name),
                );
                Js_of_ocaml.Js.Unsafe.set(
                  init_message,
                  "value",
                  Js_of_ocaml.Js.string(current_value),
                );
                post_to_iframe(projector_id, target_origin, init_message);
              | None => () /* Could not extract current value */
              };
            }
          | None => () /* Projector not found */
          }
        | None => () /* Invalid UUID format */
        };
      };
    }) {
    | _exn => () /* Ignore parse errors */
    };
    Js_of_ocaml.Js._true;
  };

  /* Try to register the message listener using unsafe coercion */
  try({
    let window = Js_of_ocaml.Dom_html.window;
    let addEventListener =
      Js_of_ocaml.Js.Unsafe.get(window, "addEventListener");
    let callback = Js_of_ocaml.Js.wrap_callback(handle_message);

    let _ =
      Js_of_ocaml.Js.Unsafe.fun_call(
        addEventListener,
        [|
          Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string("message")),
          Js_of_ocaml.Js.Unsafe.inject(callback),
        |],
      );
    ();
  }) {
  | exn =>
    Printf.printf(
      "❌ [DEBUG] addEventListener registration failed: %s\n",
      Printexc.to_string(exn),
    );
    Printf.printf(
      "🔗 External Projector Bridge initialized (no postMessage): %s\n",
      Printexc.to_string(exn),
    );
    flush_all();
  };
};
