open Util;
open Js_of_ocaml;

/**
 * Types for communication protocol between external apps and Hazel
 * Message types matching hazel-protocol.ts
 */

/* Messages sent from child (React app) to parent (Hazel) */
[@deriving (show({with_path: false}), sexp, yojson)]
type to_hazel_message =
  | Ready({id: Id.t})
  | SetSyntax({
      id: Id.t,
      codec: string,
      value: string,
    })
  | Resize({
      id: Id.t,
      width: int,
      height: int,
    })
  | RequestFocus({id: Id.t});

/* Messages sent from parent (Hazel) to child (React app) */
[@deriving (show({with_path: false}), sexp, yojson)]
type from_hazel_message =
  | Init({
      id: Id.t,
      value: string,
    })
  | Update({
      id: Id.t,
      value: string,
    });

let id_of = (msg: to_hazel_message): Id.t =>
  switch (msg) {
  | Ready({id}) => id
  | SetSyntax({id, _}) => id
  | Resize({id, _}) => id
  | RequestFocus({id}) => id
  };

let get_string = (obj: Js.t(_), key: string): option(string) =>
  try(Some(Js.to_string(Js.Unsafe.get(obj, key)))) {
  | _ => None
  };

let get_int = (obj: Js.t(_), key: string): option(int) =>
  try(Some(int_of_float(Js.float_of_number(Js.Unsafe.get(obj, key))))) {
  | _ => None
  };

/* Parse JavaScript message data directly into typed to_hazel_message */
let parse_to_hazel_message = (data: Js.t(_)): option(to_hazel_message) =>
  switch (get_string(data, "id") |> OptUtil.and_then(Id.of_string)) {
  | Some(id) =>
    switch (get_string(data, "type")) {
    | Some("ready") => Some(Ready({id: id}))
    | Some("setSyntax") =>
      switch (get_string(data, "codec"), get_string(data, "value")) {
      | (Some(codec), Some(value)) =>
        Some(
          SetSyntax({
            id,
            codec,
            value,
          }),
        )
      | _ => None
      }
    | Some("resize") =>
      switch (get_int(data, "width"), get_int(data, "height")) {
      | (Some(width), Some(height)) =>
        Some(
          Resize({
            id,
            width,
            height,
          }),
        )
      | _ =>
        prerr_endline(
          "parse_to_hazel_message: resize message missing width/height",
        );
        None;
      }
    | Some("requestFocus") => Some(RequestFocus({id: id}))
    | Some(action) =>
      prerr_endline(
        "parse_to_hazel_message: unknown message type: " ++ action,
      );
      None;
    | None =>
      prerr_endline("parse_to_hazel_message: message missing type");
      None;
    }
  | None =>
    prerr_endline("parse_to_hazel_message: missing id or wrong id format");
    None;
  };

/* Convert from_hazel_message to JavaScript object */
let from_hazel_to_js = (msg: from_hazel_message): Js.t(_) => {
  let js_obj = Js.Unsafe.obj([||]);
  let set_attr = (key, value) =>
    Js.Unsafe.set(js_obj, key, Js.string(value));
  switch (msg) {
  | Init({id, value}) =>
    set_attr("type", "init");
    set_attr("id", Id.to_string(id));
    set_attr("value", value);
  | Update({id, value}) =>
    set_attr("type", "update");
    set_attr("id", Id.to_string(id));
    set_attr("value", value);
  };
  js_obj;
};
