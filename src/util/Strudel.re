open Js_of_ocaml;

/* Check if a window property is a function */
let is_function = (name: string): bool => {
  Js.Unsafe.fun_call(
    Js.Unsafe.js_expr(
      "(function(n) { return typeof window[n] === 'function'; })",
    ),
    [|Js.Unsafe.inject(Js.string(name))|],
  )
  |> Js.to_bool;
};

/* Check if Strudel is ready (note function exists) */
let isReady: unit => bool = () => is_function("note");

/* Safe init - only call if initStrudel exists */
let initStrudel: unit => unit =
  () =>
    if (is_function("initStrudel")) {
      let fn = Js.Unsafe.js_expr("window.initStrudel");
      Js.Unsafe.fun_call(fn, [||]) |> ignore;
    };

/* Safe hush - only call if hush exists */
let hush: unit => unit =
  () =>
    if (is_function("hush")) {
      let fn = Js.Unsafe.js_expr("window.hush");
      Js.Unsafe.fun_call(fn, [||]) |> ignore;
    };

/* Abstract type for Strudel patterns */
type pattern;

/* Create a note pattern from mini-notation string */
let note: string => option(pattern) =
  s =>
    if (isReady()) {
      let noteFn = Js.Unsafe.js_expr("window.note");
      Some(
        Js.Unsafe.fun_call(noteFn, [|Js.Unsafe.inject(Js.string(s))|]),
      );
    } else {
      None;
    };

/* Reverse a pattern */
let rev: pattern => pattern = p => Js.Unsafe.meth_call(p, "rev", [||]);

/* Speed up a pattern */
let fast: (float, pattern) => pattern =
  (f, p) =>
    Js.Unsafe.meth_call(
      p,
      "fast",
      [|Js.Unsafe.inject(Js.number_of_float(f))|],
    );

/* Slow down a pattern */
let slow: (float, pattern) => pattern =
  (f, p) =>
    Js.Unsafe.meth_call(
      p,
      "slow",
      [|Js.Unsafe.inject(Js.number_of_float(f))|],
    );

/* Stack patterns (play simultaneously) */
let stack: list(pattern) => pattern =
  patterns => {
    let stackFn = Js.Unsafe.js_expr("window.stack");
    let arr = Js.array(Array.of_list(patterns));
    Js.Unsafe.fun_call(stackFn, [|Js.Unsafe.inject(arr)|]);
  };

/* Sequence patterns (play one after another) */
let seq: list(pattern) => pattern =
  patterns => {
    let seqFn = Js.Unsafe.js_expr("window.seq");
    let arr = Js.array(Array.of_list(patterns));
    Js.Unsafe.fun_call(seqFn, [|Js.Unsafe.inject(arr)|]);
  };

/* Apply jux with rev for stereo effect */
let juxRev: pattern => pattern =
  p => {
    let revFn = Js.Unsafe.js_expr("window.rev");
    Js.Unsafe.meth_call(p, "jux", [|Js.Unsafe.inject(revFn)|]);
  };

/* Play a pattern */
let play: pattern => unit =
  p => Js.Unsafe.meth_call(p, "play", [||]) |> ignore;

/* Play a note pattern - fully defensive (legacy interface) */
let playNote: string => unit =
  pattern =>
    switch (note(pattern)) {
    | Some(p) => play(juxRev(p))
    | None => ()
    };

/* Play an arbitrary pattern with jux rev stereo effect */
let playPattern: pattern => unit = p => play(juxRev(p));

/* Function to stop the music */
let stopMusic: unit => unit = () => hush();

/* Function to initialize Strudel - handles both already-loaded and loading cases */
let initOnLoad: unit => unit =
  () => {
    /* Check if initStrudel exists and call it */
    let doInit = () =>
      if (is_function("initStrudel")) {
        initStrudel();
        Printf.printf("Strudel initialized, note ready: %b\n", isReady());
      } else {
        Printf.printf("Strudel initStrudel function not found\n");
      };
    /* Check if DOM is already loaded */
    let readyState =
      Js.Unsafe.get(Js.Unsafe.js_expr("document"), "readyState")
      |> Js.to_string;
    if (readyState == "complete" || readyState == "interactive") {
      /* DOM already loaded, init immediately */
      doInit();
    } else {
      /* Wait for DOMContentLoaded */
      let cb = Js.wrap_callback(_ => doInit());
      Js.Unsafe.fun_call(
        Js.Unsafe.js_expr("window.addEventListener"),
        [|
          Js.Unsafe.inject(Js.string("DOMContentLoaded")),
          Js.Unsafe.inject(cb),
        |],
      )
      |> ignore;
    };
  };
