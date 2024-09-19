open Js_of_ocaml;

/* Direct bindings to Strudel functions */
let initStrudel: unit => unit =
  () => Js.Unsafe.coerce(Dom_html.window)##initStrudel();

let hush: unit => unit =
  () => {
    Js.Unsafe.coerce(Dom_html.window)##hush();
  };

let note: string => Js.Unsafe.any =
  pattern => {
    Js.Unsafe.coerce(Dom_html.window)##note(Js.string(pattern));
  };

let rev: Js.Unsafe.any => Js.Unsafe.any =
  pattern => {
    Js.Unsafe.coerce(Dom_html.window)##rev(pattern);
  };

let jux: (Js.Unsafe.any, Js.Unsafe.any => Js.Unsafe.any) => Js.Unsafe.any =
  (pattern, f) => {
    Js.Unsafe.coerce(pattern)##jux(f);
  };

let play: Js.Unsafe.any => unit =
  pattern => {
    Js.Unsafe.coerce(pattern)##play();
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
let initOnLoad: unit => Dom_events.listener =
  () => {
    Dom_events.listen(
      Dom_html.window,
      Dom_events.Typ.domContentLoaded,
      (_: Js.t(Dom_html.window), _: Js.t(Dom_html.event)) => {
        initStrudel();
        false; // I don't think this does anything.
      },
    );
  };
