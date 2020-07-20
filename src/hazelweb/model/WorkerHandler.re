module Js = Js_of_ocaml.Js;
/* type t = Lwt.t; */

let run = _data => {
  Js.Unsafe.eval_string(
    {|
    var worker = new Worker( "Worker.js" );

    //Set up client side worker interaction
    worker.onmessage = function(e) {
      console.log("Received: " + e.data);
    }
    worker.postMessage(4);
    |},
  );
};
