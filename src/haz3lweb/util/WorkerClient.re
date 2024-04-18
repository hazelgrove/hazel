open Js_of_ocaml;
open WorkerServer;

let fetch_data = (url, callback) => {
  open XmlHttpRequest;
  let request = create();
  request##_open(Js.string("GET"), Js.string(url), Js._true);
  request##.onreadystatechange :=
    Js.wrap_callback(_ =>
      switch (request##.readyState) {
      | DONE =>
        if (request##.status == 200) {
          let data = request##.response;
          let blob =
            Js.Opt.get(File.CoerceTo.blob(data), () => assert(false));
          let blob_url = Js.Unsafe.global##._URL##createObjectURL(blob);
          callback(Some(blob_url));
        } else {
          callback(None);
        }
      | _ => ()
      }
    );
  request##send(Js.null);
};

let blob_url = ref(None);
let url = "https://hazel.org/build/dev/worker.js"; // Worker file name

let () =
  fetch_data("https://hazel.org/build/dev/worker.js", result => {
    blob_url := result
  });

let timeoutDuration = 20000; // Worker timeout in ms

let rec initWorker = () => {
  /* Start polling the ref */
  switch (blob_url^) {
  | Some(url) => Worker.create(url)
  | None =>
    Dom_html.setTimeout(() => (), 100.) |> ignore;
    initWorker();
  };
};

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
