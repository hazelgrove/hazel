open Js_of_ocaml;
open WorkerServer;

let name = "worker.js"; // Worker file name
let timeoutDuration = 20000; // Worker timeout in ms

let initWorker: unit => Js.t(Worker.worker(Request.t, Response.t)) =
  () => Worker.create(name);

let workerRef: ref(option(Js.t(Worker.worker(Request.t, Response.t)))) =
  ref(None);

let timeoutId = ref(None);

let getWorker = (): Js.t(Worker.worker(Request.t, Response.t)) => {
  switch (workerRef.contents) {
  | Some(w) => w
  | None =>
    let w = initWorker();
    workerRef.contents = Some(w);
    w;
  };
};

let restart_worker = (): unit => {
  switch (workerRef.contents) {
  | Some(w) => w##terminate
  | None => ()
  };
  let w = initWorker();
  workerRef.contents = Some(w);
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
        evt##.data |> handler;
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

  setupWorkerMessageHandler(getWorker());

  getWorker()##postMessage(req);

  let onTimeout = (): unit => {
    restart_worker();
    setupWorkerMessageHandler(getWorker());
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

let has_pending_request = () => timeoutId.contents != None;

let request = (req, ~handler, ~timeout) =>
  switch (req) {
  | [] => ()
  | _ => request(req, ~handler, ~timeout)
  };
