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

/* JSON Codec for converting between Hazel expressions and Yojson */
module JsonCodec = {
  /* Stage 1: Support only integers; stub out other types with clear errors */

  let exp_to_yojson =
      (exp: Language.Term.Exp.t): result(Yojson.Safe.t, string) => {
    switch (exp.term) {
    | Atom(Int(i)) =>
      switch (Bigint.to_int(i)) {
      | Some(int_val) => Ok(`Int(int_val))
      | None => Error("Integer too large to convert to JSON")
      }
    | Atom(Float(_)) => Error("Float values not yet supported in JsonCodec")
    | Atom(String(_)) =>
      Error("String values not yet supported in JsonCodec")
    | Atom(Bool(_)) => Error("Bool values not yet supported in JsonCodec")
    | ListLit(_) => Error("List values not yet supported in JsonCodec")
    | Tuple(_) => Error("Tuple values not yet supported in JsonCodec")
    | Parens(_) => Error("Parens values not yet supported in JsonCodec")
    | Constructor(_) =>
      Error("Constructor values not yet supported in JsonCodec")
    | _ => Error("Unsupported expression type for JsonCodec")
    };
  };

  let any_to_yojson =
      (any: Language.Term.Any.t): result(Yojson.Safe.t, string) => {
    switch (any) {
    | Exp(exp) => exp_to_yojson(exp)
    | _ => Error("Only Exp terms are supported in JsonCodec")
    };
  };

  let yojson_to_exp =
      (json: Yojson.Safe.t): result(Language.Term.Exp.t, string) => {
    switch (json) {
    | `Int(i) =>
      try({
        let _big_int = Bigint.of_int(i);
        Ok(Language.IdTagged.FreshGrammar.Exp.big_int(Bigint.of_int(i)));
      }) {
      | _ => Error("Failed to convert int to Bigint")
      }
    | `Float(_) => Error("Float values not yet supported in JsonCodec")
    | `String(_) => Error("String values not yet supported in JsonCodec")
    | `Bool(_) => Error("Bool values not yet supported in JsonCodec")
    | `List(_) => Error("List values not yet supported in JsonCodec")
    | `Assoc(_) => Error("Object values not yet supported in JsonCodec")
    | `Null => Error("Null values not supported in JsonCodec")
    | `Tuple(_) => Error("Tuple values not yet supported in JsonCodec")
    | `Intlit(_) => Error("Intlit values not yet supported in JsonCodec")
    | `Variant(_) => Error("Variant values not yet supported in JsonCodec")
    };
  };

  let yojson_to_any =
      (json: Yojson.Safe.t): result(Language.Term.Any.t, string) => {
    switch (yojson_to_exp(json)) {
    | Ok(exp) => Ok(Exp(exp))
    | Error(msg) => Error(msg)
    };
  };
};
