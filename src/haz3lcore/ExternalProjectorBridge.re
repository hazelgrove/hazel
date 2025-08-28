open Util;
open ProjectorBase;

/* Global registry to store parent callbacks by projector ID */
let parent_callbacks: ref(Id.Map.t(external_action => Ui_effect.t(unit))) =
  ref(Id.Map.empty);

let register_projector =
    (id: Id.t, parent_callback: external_action => Ui_effect.t(unit)): unit => {
  parent_callbacks := Id.Map.add(id, parent_callback, parent_callbacks^);
  Printf.printf("🔗 Registered projector %s\n", Id.cls(id));
  flush_all();
};

let unregister_projector = (id: Id.t): unit => {
  parent_callbacks := Id.Map.remove(id, parent_callbacks^);
  Printf.printf("🔗 Unregistered projector %s\n", Id.cls(id));
  flush_all();
};

/* Basic postMessage listener that just logs for now */
let init = (): unit => {
  let handle_message = event => {
    /* Check if this is from our slider */
    let origin = Js_of_ocaml.Js.to_string(event##.origin);
    if (origin == "http://localhost:5173") {
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

        Printf.printf(
          "📨 PostMessage: type=%s, id=%s, value=%s\n",
          msg_type,
          msg_id,
          msg_value,
        );
        flush_all();
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
  try({
    let window = Js_of_ocaml.Dom_html.window;
    let addEventListener =
      Js_of_ocaml.Js.Unsafe.get(window, "addEventListener");
    let _ =
      Js_of_ocaml.Js.Unsafe.fun_call(
        addEventListener,
        [|
          Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string("message")),
          Js_of_ocaml.Js.Unsafe.inject(
            Js_of_ocaml.Js.wrap_callback(handle_message),
          ),
        |],
      );
    Printf.printf(
      "🔗 External Projector Bridge with postMessage listener initialized\n",
    );
    flush_all();
  }) {
  | exn =>
    Printf.printf(
      "🔗 External Projector Bridge initialized (no postMessage): %s\n",
      Printexc.to_string(exn),
    );
    flush_all();
  };
};
