open Js_of_ocaml;

Js.Unsafe.js_expr("require('process')");
let arg_str = Js.Unsafe.js_expr("process.argv[2]") |> Js.to_string;
print_endline("LSP: recieved string: " ++ arg_str);
Haz3lcore.TyDi.lsp(arg_str);
