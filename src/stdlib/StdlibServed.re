open Js_of_ocaml;

/* The baked library as files served beside the app (src/web/www): fetched
   once and cached apart from the code, rather than embedded in both
   bundles, so a bigger library doesn't make a first load compile more
   script. Node entry points use StdlibBaked. */

let blob_file = "stdlib.blob";
let source_file = "stdlib.hz";

/* each step timed as a performance measure, for the boot benchmark */
let timed = (name: string, f: unit => 'a): 'a => {
  let perf = Js.Unsafe.global##.performance;
  let start = "stdlib:" ++ name ++ ":start";
  perf##mark(Js.string(start)) |> ignore;
  let r = f();
  perf##measure(Js.string("stdlib:" ++ name), Js.string(start)) |> ignore;
  r;
};

/* [url]'s text, or None; synchronous only off the main thread */
let get = (~sync=false, url: string, k: option(Js.t(Js.js_string)) => unit) => {
  let xhr = XmlHttpRequest.create();
  let finish = () =>
    k(xhr##.status == 200 ? Js.Opt.to_option(xhr##.responseText) : None);
  xhr##_open(Js.string("GET"), Js.string(url), Js.bool(!sync));
  if (!sync) {
    xhr##.onreadystatechange :=
      Js.wrap_callback(() =>
        if (xhr##.readyState == XmlHttpRequest.DONE) {
          finish();
        }
      );
  };
  xhr##send(Js.null);
  if (sync) {
    finish();
  };
};

/* the blob is base64: atob is fast in browsers and workers alike */
let install = (b64: Js.t(Js.js_string)) => {
  let raw =
    timed("atob", () => Js.to_bytestring(Js.Unsafe.global##atob(b64)));
  let baked = timed("decode", () => Language.Library.decode(raw));
  timed("install", () => Language.Library.install_baked(baked));
};

let missing = file =>
  Js.Unsafe.global##.console##error(
    Js.string("stdlib: could not load " ++ file),
  );

/* for the worker, before it takes requests */
let load_sync = () =>
  get(~sync=true, blob_file, b64 =>
    switch (b64) {
    | Some(b64) => install(b64)
    | None => missing(blob_file)
    }
  );

/* installed, then [k] with the shipped source ("" if it didn't load) */
let load = (k: string => unit) => {
  let source = ref(None);
  let installed = ref(false);
  let ready = () =>
    switch (source^) {
    | Some(text) when installed^ => k(text)
    | _ => ()
    };
  get(
    blob_file,
    b64 => {
      switch (b64) {
      | Some(b64) => install(b64)
      | None => missing(blob_file)
      };
      installed := true;
      ready();
    },
  );
  get(
    source_file,
    text => {
      if (text == None) {
        missing(source_file);
      };
      source := Some(Option.fold(~none="", ~some=Js.to_string, text));
      ready();
    },
  );
};
