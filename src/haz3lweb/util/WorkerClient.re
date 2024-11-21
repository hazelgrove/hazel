open Js_of_ocaml;
open WorkerServer;

let name = "worker.js"; // Worker file name
let timeoutDuration = 20000; // Worker timeout in ms
let secondaryTimeoutDuration = 100; // Secondary timeout for ongoing requests

let initWorker = () => Worker.create(name);

let workerRef: ref(Js.t(Worker.worker(string, string))) =
  ref(initWorker());

let timeoutId = ref(None);
let secondaryTimeoutId = ref(None);

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
        switch (secondaryTimeoutId.contents) {
        | Some(id) => Dom_html.window##clearTimeout(id)
        | None => ()
        };
        timeoutId.contents = None; /* Clear primary timeout */
        secondaryTimeoutId.contents = None; /* Clear secondary timeout */
        evt##.data |> Response.deserialize |> handler;
        Js._true;
      });
  };

  /* If there's an ongoing request, use the secondary timeout */
  switch (timeoutId.contents) {
  | Some(primaryId) =>
    Dom_html.window##clearTimeout(primaryId);

    // because restart_worker is expensive, only do it if the previous request takes a long time
    let onSecondaryTimeout = (): unit => {
      print_endline(
        "Secondary timeout triggered: Restarting worker and resending request.",
      );
      restart_worker();
      setupWorkerMessageHandler(workerRef.contents);
      workerRef.contents##postMessage(Request.serialize(req));
    };

    secondaryTimeoutId.contents =
      Some(
        Dom_html.window##setTimeout(
          Js.wrap_callback(onSecondaryTimeout),
          float_of_int(secondaryTimeoutDuration),
        ),
      );

  | None => ()
  };

  setupWorkerMessageHandler(workerRef.contents);

  workerRef.contents##postMessage(Request.serialize(req));

  let onTimeout = (): unit => {
    print_endline(
      "Primary timeout triggered: Restarting worker and resending request.",
    );
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
