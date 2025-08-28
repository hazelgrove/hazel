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
  let entry = {
    parent_callback,
    info,
  };
  projector_registry := Id.Map.add(id, entry, projector_registry^);
};

let unregister_projector = (id: Id.t): unit => {
  projector_registry := Id.Map.remove(id, projector_registry^);
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

/* Full postMessage handler with codec support */
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
        let msg_codec = get_string(data, "codec");

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
                  parent_callback(SetSyntax(new_segment))
                | None => ()
                }
              | _ => () /* Unknown codec */
              }
            | None => () /* Projector not found */
            }
          | None => () /* Invalid UUID format */
          };
        };
      }) {
      | exn => () /* Ignore parse errors */
      };
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
