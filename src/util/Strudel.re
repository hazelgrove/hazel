open Js_of_ocaml;

/* Strudel.re */
/* Strudel.re */

/* Direct bindings to Strudel functions */
let initStrudel: unit => unit =
  () => {
    let initStrudelFn = Js.Unsafe.js_expr("window.initStrudel");
    Js.Unsafe.fun_call(initStrudelFn, [||]);
  };

let hush: unit => unit =
  () => {
    let hushFn = Js.Unsafe.js_expr("window.hush");
    Js.Unsafe.fun_call(hushFn, [||]);
  };

let note: string => Js.Unsafe.any =
  pattern => {
    let noteFn = Js.Unsafe.js_expr("window.note");
    Js.Unsafe.fun_call(noteFn, [|Js.Unsafe.inject(Js.string(pattern))|]);
  };

let rev: Js.Unsafe.any => Js.Unsafe.any =
  pattern => {
    let revFn = Js.Unsafe.js_expr("window.rev");
    Js.Unsafe.fun_call(revFn, [|Js.Unsafe.inject(pattern)|]);
  };

let jux: (Js.Unsafe.any, Js.Unsafe.any => Js.Unsafe.any) => Js.Unsafe.any =
  (pattern, f) => {
    Js.Unsafe.meth_call(pattern, "jux", [|Js.Unsafe.inject(f)|]);
  };

let play: Js.Unsafe.any => unit =
  pattern => {
    Js.Unsafe.meth_call(pattern, "play", [||]);
  };

/* Wrapper function to chain methods */
let playNote: string => unit =
  pattern => {
    let n = note(pattern);
    let j = jux(n, rev);
    play(j);
  };

/* Example usage function */
let exampleUse: unit => unit =
  () => {
    initStrudel();
    playNote("<c a f e>(3,8)");
  };

/* Function to stop the music */
let stopMusic: unit => unit = () => hush();

/* Function to initialize Strudel when the DOM is loaded */
let initOnLoad: unit => unit =
  () => {
    let addEventListenerFn = Js.Unsafe.js_expr("window.addEventListener");
    Js.Unsafe.fun_call(
      addEventListenerFn,
      [|
        Js.Unsafe.inject(Js.string("DOMContentLoaded")),
        Js.Unsafe.inject(Js.wrap_callback(_ => initStrudel())),
      |],
    );
  };
