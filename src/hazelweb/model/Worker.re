module Js = Js_of_ocaml.Js;
/* type t = Lwt.t; */

let _ = {
  Js.export("workerFunction", [%js {as _; pub f = Synthesizer.f}]);
  Js.Unsafe.eval_string(
    {|
    postMessage(17);

    /************************* BEGIN WORKER CODE **************************/
    onmessage = function(e) {
        postMessage(workerFunction.f(e.data));
    };
    /************************* END WORKER CODE **************************/
    |},
  );
};
