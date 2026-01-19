open Js_of_ocaml;
open WorkerServer;

let name = "worker.js"; // Worker file name
let timeoutDuration = 20000; // Worker timeout in ms

let initWorker: unit => Js.t(Worker.worker(Request.t, Response.t)) =
  () => Worker.create(name);

let workerRef: ref(Js.t(Worker.worker(Request.t, Response.t))) =
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
        /* Deserialize BigInts that were serialized for structured clone */
        evt##.data |> BigIntShim.deserialize |> handler;
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

  /* Serialize BigInts before posting (they don't survive structured clone) */
  workerRef.contents##postMessage(BigIntShim.serialize(req));

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

let request = (req, ~handler, ~timeout) =>
  switch (req) {
  | [] => ()
  | _ => request(req, ~handler, ~timeout)
  };
