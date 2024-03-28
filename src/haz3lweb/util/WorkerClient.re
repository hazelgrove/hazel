open Js_of_ocaml;
open WorkerServer;

let name = "worker.js"; // Worker file name
let timeoutDuration = 20000; // Worker timeout in ms

let initWorker = () => Worker.create(name);

let workerRef: ref(Js.t(Worker.worker(string, string))) =
  ref(initWorker());

let timeoutId = ref(None);

let restart_worker = (): unit => {
  workerRef.contents##terminate;
  workerRef.contents = initWorker();
};

let request =
    (
      req: Request.t,
      ~handler: Response.t => unit,
      ~timeout: Request.t => unit,
    )
    : unit => {
  let setupWorkerMessageHandler = worker => {
    worker##.onmessage :=
      Dom.handler(evt => {
        switch (timeoutId.contents) {
        | Some(id) => Dom_html.window##clearTimeout(id)
        | None => ()
        };
        timeoutId.contents = None; /* Clear timeout after response */
        evt##.data |> Response.deserialize |> handler;
        Js._true;
      });
  };

  /* If there's an ongoing request, terminate the worker and reinitialize */
  switch (timeoutId.contents) {
  | Some(id) =>
    Dom_html.window##clearTimeout(id);
    restart_worker();
  | None => ()
  };

  setupWorkerMessageHandler(workerRef.contents);

  // print_endline(Request.serialize(req));

  workerRef.contents##postMessage(Request.serialize(req));

  let onTimeout = (): unit => {
    restart_worker();
    setupWorkerMessageHandler(workerRef.contents);
    timeout(req);
  };

  timeoutId.contents =
    Some(
      Dom_html.window##setTimeout(
        Js.wrap_callback(onTimeout),
        float_of_int(timeoutDuration),
      ),
    );
};
