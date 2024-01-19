open Js_of_ocaml;
open Tezt;

include Test_Elaborator;
include TeztExample;

let main = (_s: string) => {
  Test.run();
};
Js.Unsafe.js_expr("require('process')");
Js.Unsafe.js_expr("process.argv[2]") |> Js.to_string |> main;
