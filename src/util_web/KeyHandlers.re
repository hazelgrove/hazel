/* SPIKE (wasm-eval-bench): Key.handler / Key.listener moved out of Key so
   that Key stays js_of_ocaml-only in the Bonsai-free [util] library. */
open Util;
open Key;

/* Keyboard event handler for focusable components.
 * Adds tabindex(0) so the element can receive focus and key events. */
let handler = (~f: t => Virtual_dom.Vdom.Effect.t(unit)) =>
  Virtual_dom.Vdom.(
    Attr.many([
      Attr.on_keydown(evt => f(mk(KeyDown, evt))),
      Attr.on_keyup(evt => f(mk(KeyUp, evt))),
      Attr.tabindex(0),
    ])
  );

/* Keyboard event listener without tabindex.
 * For elements that catch bubbled key events (e.g. a page-level
 * container) but shouldn't themselves become focusable. */
let listener = (~f: t => Virtual_dom.Vdom.Effect.t(unit)) =>
  Virtual_dom.Vdom.(
    Attr.many([
      Attr.on_keydown(evt => f(mk(KeyDown, evt))),
      Attr.on_keyup(evt => f(mk(KeyUp, evt))),
    ])
  );
