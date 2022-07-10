open Js_of_ocaml;
open Sexplib;

let get_program_result = (program: Program.t) => {
  let worker = Worker.create("./test.js");
  worker##postMessage(Sexp.to_string(Program.sexp_of_t(program)));
  Program.get_result(program);
};
