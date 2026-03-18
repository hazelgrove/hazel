open Js_of_ocaml;
open WorkerServer;

let timeoutDuration = 20000; // Worker timeout in ms

/* Capture script src at module load time (document.currentScript is only
   available during synchronous execution). In patchwork direct mode this
   gives us the full URL to hazel.js so we can resolve worker.js next to it. */
let captured_script_src: option(string) = {
  let cs = Js.Unsafe.global##.document##.currentScript;
  if (Js.Opt.test(cs)) {
    let src = Js.Unsafe.get(cs, "src");
    if (Js.Optdef.test(src)) {
      Some(Js.to_string(src));
    } else {
      None;
    };
  } else {
    None;
  };
};

let worker_url = (): string =>
  switch (captured_script_src) {
  | Some(src) when String.length(src) > 0 =>
    Js_of_ocaml.Regexp.global_replace(
      Js_of_ocaml.Regexp.regexp("hazel\\.js$"),
      src,
      "worker.js",
    )
  | _ => "worker.js"
  };

let initWorker: unit => Js.t(Worker.worker(Request.t, Response.t)) =
  () => Worker.create(worker_url());

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

  setupWorkerMessageHandler(workerRef.contents);

  workerRef.contents##postMessage(req);

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
