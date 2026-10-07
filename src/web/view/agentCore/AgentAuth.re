open Util;
open Js_of_ocaml;

[@deriving (show({with_path: false}), sexp, yojson)]
type browser_key = {
  key: option(string),
  remember: bool,
  error: bool,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type connection = {
  key: option(string),
  remember_browser: bool,
  remember_local: bool,
  error: option(string),
};

let api = () => Js.Unsafe.get(Js.Unsafe.global, "hazelAgentAuth");
let string_arg = value => Js.Unsafe.inject(Js.string(value));
let bool_arg = value => Js.Unsafe.inject(Js.bool(value));
let call = (name, args) => Js.Unsafe.meth_call(api(), name, args);

let load_browser = legacy_key =>
  try({
    let json =
      call(
        "loadBrowser",
        [|string_arg(Option.value(~default="", legacy_key))|],
      )
      |> Js.to_string
      |> Yojson.Safe.from_string;
    browser_key_of_yojson(json);
  }) {
  | _ => {
      key: legacy_key,
      remember: false,
      error: true,
    }
  };

let save_browser = (key, remember) =>
  try(
    call(
      "saveBrowser",
      [|string_arg(Option.value(~default="", key)), bool_arg(remember)|],
    )
    |> Js.to_bool
  ) {
  | _ => false
  };

let clear_editor_storage = () =>
  try(ignore(call("clearEditorStorage", [||]))) {
  | _ => ()
  };

let begin_connection = (remember_browser, remember_local, on_error) =>
  try(
    ignore(
      call(
        "begin",
        [|
          bool_arg(remember_browser),
          bool_arg(remember_local),
          Js.Unsafe.inject(
            Js.wrap_callback(error => on_error(Js.to_string(error))),
          ),
        |],
      ),
    )
  ) {
  | _ =>
    on_error(
      "OpenRouter connection is unavailable. You can enter a key manually.",
    )
  };

let finish_connection = handler =>
  try(
    ignore(
      call(
        "finish",
        [|
          Js.Unsafe.inject(
            Js.wrap_callback(raw => {
              let result =
                try(
                  switch (Js.to_string(raw) |> Yojson.Safe.from_string) {
                  | `Null => None
                  | json => Some(connection_of_yojson(json))
                  }
                ) {
                | _ => None
                };
              handler(result);
            }),
          ),
        |],
      ),
    )
  ) {
  | _ => handler(None)
  };
