open Util;
open ProjectorBase;
open Language;

/* Registry entry storing both callback and projector info for codec operations */
[@deriving (show({with_path: false}), sexp, yojson)]
type projector_entry = {
  parent_callback: ProjectorBase.external_action => unit,
  info: ProjectorBase.info,
};

/* Global registry to store projector entries by ID */
let projector_registry: ref(Id.Map.t(projector_entry)) = ref(Id.Map.empty);

let register_projector =
    (
      id: Id.t,
      parent_callback: ProjectorBase.external_action => unit,
      info: ProjectorBase.info,
    )
    : unit => {
  /* Wrap the parent callback with debugging */
  let debug_parent_callback = (action: ProjectorBase.external_action) => {
    Printf.printf(
      "🎪 [DEBUG] parent_callback wrapper called with action\n",
    );
    flush_all();

    /* Print the projector ID this callback is for */
    Printf.printf(
      "🎪 [DEBUG] This is the callback for projector: %s\n",
      Id.to_string(id),
    );
    flush_all();

    let result = parent_callback(action);
    Printf.printf("🎪 [DEBUG] parent_callback returned\n");
    flush_all();
    result;
  };

  let entry = {
    parent_callback: debug_parent_callback,
    info,
  };
  projector_registry := Id.Map.add(id, entry, projector_registry^);
  Printf.printf("🔗 Registered projector %s\n", Id.cls(id));
  flush_all();
};

let unregister_projector = (id: Id.t): unit => {
  projector_registry := Id.Map.remove(id, projector_registry^);
  Printf.printf("🔗 Unregistered projector %s\n", Id.cls(id));
  flush_all();
};

/* Integer codec to convert JSON string to Hazel integer segment */
module IntCodec = {
  let json_to_segment =
      (info: ProjectorBase.info, value_str: string): option(Base.segment) =>
    try({
      let int_val = Bigint.of_string(value_str);
      info.utility.lift_syntax(
        fun
        | Exp(t) =>
          Exp({
            ...t,
            term: Atom(Int(int_val)),
          })
        | _ => failwith("not an int literal"),
        info.syntax,
      );
    }) {
    | _ => None
    };
};

/* Test function to verify event listener is working - callable from browser console */
let test_listener = () => {
  Printf.printf(
    "🧪 [TEST] test_listener called - this proves global function exposure works\n",
  );
  Printf.printf(
    "🧪 [TEST] If you can see this, then Js.Unsafe is working for globals\n",
  );
  flush_all();
};

/* Full postMessage handler with codec support */
let init = (): unit => {
  let handle_message = event => {
    Printf.printf("🎯 [DEBUG] handle_message called! Event received\n");
    flush_all();

    /* Check if this is from our slider */
    let origin = Js_of_ocaml.Js.to_string(event##.origin);
    Printf.printf("🎯 [DEBUG] Message origin: %s\n", origin);
    flush_all();

    if (origin == "http://localhost:5173") {
      Printf.printf("🎯 [DEBUG] Origin matches - processing message\n");
      flush_all();
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

        Printf.printf(
          "📨 PostMessage: type=%s, id=%s, codec=%s, value=%s\n",
          msg_type,
          msg_id,
          msg_codec,
          msg_value,
        );
        flush_all();

        /* Handle setSyntax messages */
        if (msg_type == "setSyntax") {
          switch (Id.of_string(msg_id)) {
          | Some(projector_id) =>
            switch (Id.Map.find_opt(projector_id, projector_registry^)) {
            | Some({parent_callback, info}) =>
              switch (msg_codec) {
              | "int" =>
                switch (IntCodec.json_to_segment(info, msg_value)) {
                | Some(new_segment) =>
                  /* Use parent_callback to dispatch SetSyntax */
                  Printf.printf(
                    "🚀 About to call parent_callback with SetSyntax\n",
                  );
                  flush_all();
                  let effect = parent_callback(SetSyntax(new_segment));
                  Printf.printf("🚀 parent_callback returned an effect\n");
                  flush_all();
                  Printf.printf(
                    "✅ SetSyntax dispatched for projector %s (ID: %s)\n",
                    msg_id,
                    Id.to_string(projector_id),
                  );
                  flush_all();
                | None =>
                  Printf.printf(
                    "❌ Failed to convert int   value: %s\n",
                    msg_value,
                  );
                  flush_all();
                }
              | _ =>
                Printf.printf("❌ Unknown codec: %s\n", msg_codec);
                flush_all();
              }
            | None =>
              Printf.printf("❌ Projector not found: %s\n", msg_id);
              flush_all();
            }
          | None =>
            Printf.printf("❌ Invalid UUID format: %s\n", msg_id);
            flush_all();
          };
        };
      }) {
      | exn =>
        Printf.printf(
          "📨 PostMessage parse error: %s\n",
          Printexc.to_string(exn),
        );
        flush_all();
      };
    };
    Js_of_ocaml.Js._true;
  };

  /* Try to register the message listener using unsafe coercion */
  try(
    {
      Printf.printf("🔧 [DEBUG] Starting addEventListener registration\n");
      flush_all();

      let window = Js_of_ocaml.Dom_html.window;
      Printf.printf("🔧 [DEBUG] Got window object\n");
      flush_all();

      let addEventListener =
        Js_of_ocaml.Js.Unsafe.get(window, "addEventListener");
      Printf.printf("🔧 [DEBUG] Got addEventListener function\n");
      flush_all();

      let callback = Js_of_ocaml.Js.wrap_callback(handle_message);
      Printf.printf("🔧 [DEBUG] Wrapped callback function\n");
      flush_all();

      let _ =
        Js_of_ocaml.Js.Unsafe.fun_call(
          addEventListener,
          [|
            Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string("message")),
            Js_of_ocaml.Js.Unsafe.inject(callback),
          |],
        );
      Printf.printf(
        "🔧 [DEBUG] addEventListener call completed successfully\n",
      );
      flush_all();

      Printf.printf(
        "🔗 External Projector Bridge with postMessage listener initialized\n",
      );
      flush_all();

      /* Expose test function to global scope for debugging */
      let global = Js_of_ocaml.Js.Unsafe.global;
      Js_of_ocaml.Js.Unsafe.set(
        global,
        "hazelTestListener",
        Js_of_ocaml.Js.wrap_callback(test_listener),
      );
      Printf.printf(
        "🧪 [DEBUG] Exposed hazelTestListener() to global scope\n",
      );
      flush_all();
    }
  ) {
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
