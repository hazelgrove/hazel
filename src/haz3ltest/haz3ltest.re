open Alcotest;
open Js_of_ocaml;
open Test_Elaboration;

let main = (_s: string) => {
  run("Dynamics", [("Elaboration", elaboration_tests)]);
};

Js.Unsafe.js_expr("require('process')");
Js.Unsafe.js_expr("process.argv[2]") |> Js.to_string |> main;
