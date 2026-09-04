/* SPIKE (wasm-eval-bench): the only time primitives [language] needs, split
   out of JsUtil so that JsUtil -- full of DOM code whose typed js_of_ocaml
   bindings changed between 5.x and 6.x -- can live in [util_web] and stay
   out of the wasm build entirely.

   Both go through Js.Unsafe and are annotated [float] on purpose: the typed
   bindings return [float] under js_of_ocaml 5.x but [Js.number_t] under
   6.x, and this source has to compile under both. */
open Js_of_ocaml;

let timestamp = (): float =>
  Js.Unsafe.meth_call(Js.Unsafe.global##.Date, "now", [||]);

let precise_timestamp = (): float =>
  Js.Unsafe.meth_call(Js.Unsafe.global##.performance, "now", [||]);
