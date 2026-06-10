open Util;
open Js_of_ocaml;

/* Js keyboard-event extraction for [Util.Key]: builds the pure
 * [Key.t] record from DOM keyboard events and provides Vdom
 * keydown/keyup attributes. */

let get_key = evt =>
  Js.to_string(Js.Optdef.get(evt##.key, () => failwith("KeyEvent.get_key")));

let ctrl_held = evt => Js.to_bool(evt##.ctrlKey);
let shift_held = evt => Js.to_bool(evt##.shiftKey);
let alt_held = evt => Js.to_bool(evt##.altKey);
let meta_held = evt => Js.to_bool(evt##.metaKey);

let key_of = (dir: Key.dir, evt): Key.key => {
  let key = get_key(evt);
  switch (dir) {
  | KeyUp => U(key)
  | KeyDown => D(key)
  };
};

let get_code = evt =>
  Js.to_string(Js.Optdef.get(evt##.code, () => Js.string("")));

let mk = (dir, evt): Key.t => {
  key: key_of(dir, evt),
  code: get_code(evt),
  sys: Os.is_mac^ ? Mac : PC,
  shift: Key.to_held(shift_held(evt)),
  meta: Key.to_held(meta_held(evt)),
  ctrl: Key.to_held(ctrl_held(evt)),
  alt: Key.to_held(alt_held(evt)),
};

/* Keyboard event handler for focusable components.
 * Adds tabindex(0) so the element can receive focus and key events. */
let handler = (~f: Key.t => Virtual_dom.Vdom.Effect.t(unit)) =>
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
let listener = (~f: Key.t => Virtual_dom.Vdom.Effect.t(unit)) =>
  Virtual_dom.Vdom.(
    Attr.many([
      Attr.on_keydown(evt => f(mk(KeyDown, evt))),
      Attr.on_keyup(evt => f(mk(KeyUp, evt))),
    ])
  );
