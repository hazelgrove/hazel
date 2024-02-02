open Js_of_ocaml;
open WorkerServer;

let name = "worker.js"; // Worker file name
let timeoutDuration = 20000; // Worker timeout in ms

let initWorker = () => Worker.create(name);

let workerRef: ref(Js.t(Worker.worker(string, string))) =
  ref(initWorker());

let timeoutId = ref(None);

let request = (req: Request.t, handler: Response.t => unit): unit => {
  let setupWorkerMessageHandler = worker => {
    worker##.onmessage :=
      Dom.handler(evt => {
        switch (timeoutId.contents) {
        | Some(id) => Dom_html.window##clearTimeout(id)
        | None => ()
        };
        timeoutId.contents = None; /* Clear timeout after response */
        let res = evt##.data |> Response.deserialize;
        handler(res);
        Js._true;
      });
  };

  /* If there's an ongoing request, terminate the worker and reinitialize */
  switch (timeoutId.contents) {
  | Some(id) =>
    Dom_html.window##clearTimeout(id);
    workerRef.contents##terminate;
    workerRef.contents = initWorker();
  | None => ()
  };

  setupWorkerMessageHandler(workerRef.contents);

  workerRef.contents##postMessage(req |> Request.serialize);

  let onTimeout = () => {
    workerRef.contents##terminate;
    workerRef.contents = initWorker();
    setupWorkerMessageHandler(workerRef.contents);
  };

  timeoutId.contents =
    Some(
      Dom_html.window##setTimeout(
        Js.wrap_callback(onTimeout),
        float_of_int(timeoutDuration),
      ),
    );
};
