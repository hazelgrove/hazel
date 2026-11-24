open Js_of_ocaml;

/* base64 (the embedded JS string stays unescaped); decoded by the
   platform's atob */
let blob = [%blob "stdlib.blob"];

/* for node (the CLI, tests); the app loads StdlibServed's files */
let install = () =>
  Js.to_bytestring(Js.Unsafe.global##atob(Js.string(blob)))
  |> Language.Library.install;
