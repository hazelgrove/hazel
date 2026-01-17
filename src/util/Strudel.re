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

/* Play a note pattern - fully defensive */
let playNote: string => unit =
  pattern =>
    if (isReady()) {
      /* Get the note function and create a pattern */
      let noteFn = Js.Unsafe.js_expr("window.note");
      let n =
        Js.Unsafe.fun_call(
          noteFn,
          [|Js.Unsafe.inject(Js.string(pattern))|],
        );
      /* Apply jux with rev for stereo effect */
      let revFn = Js.Unsafe.js_expr("window.rev");
      let j = Js.Unsafe.meth_call(n, "jux", [|Js.Unsafe.inject(revFn)|]);
      /* Play the pattern */
      Js.Unsafe.meth_call(j, "play", [||]) |> ignore;
    } else {
      Printf.printf("Strudel not ready - window.note not available\n");
    };

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
