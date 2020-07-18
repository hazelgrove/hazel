module Js = Js_of_ocaml.Js;
/* type t = Lwt.t; */

let run = _data => {
  Js.export("workerFunction", [%js {as _; pub f = Synthesizer.f}]);
  Js.Unsafe.eval_string(
    {|
    var serialized_worker_function = workerFunction.f.toString();

    var worker_code2 = "self.onmessage = function(e) {\
        self.postMessage(("+serialized_worker_function+")(e.data))\
    }"

    //Declare the worker as an anonymous function body string
    var worker_code = "("+function(){
    /************************* BEGIN WORKER CODE **************************/
    self.onmessage = function(e) {
        self.postMessage(e.data+" world");
    };
    /************************* END WORKER CODE **************************/
    }.toString()+")()";

    //Debug
    console.log(worker_code);
    console.log(worker_code2);

    // Build a worker from that string
    var blobURL = URL.createObjectURL( new Blob([worker_code], { type: 'text/javascript' } ) );
    var worker = new Worker( blobURL );

    //Do not revoke the blobURL
    //URL.revokeObjectURL( blobURL );

    //Set up client side worker interaction
    worker.onmessage = function(e) {
    console.log("Received: " + e.data);
    }
    console.log(workerFunction.f)
    worker.postMessage("Hello");
    |},
  );
};
