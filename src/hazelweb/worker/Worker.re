open Js_of_ocaml;
open Sexplib;

let get_result_promise = (program: Program.t) => {
  let worker = Worker.create("./test.js");
  worker##postMessage(Sexp.to_string(Program.sexp_of_t(program)));
  let onMessage = evt => {
    JSUtil.log(evt##.data);
    Js._true;
  };
  worker##.onmessage := Dom.handler(onMessage);
};
