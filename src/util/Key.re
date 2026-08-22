open Ppx_yojson_conv_lib.Yojson_conv;
open Js_of_ocaml;

[@deriving (show({with_path: false}), yojson)]
type dir =
  | KeyUp
  | KeyDown;

[@deriving (show({with_path: false}), yojson)]
type key =
  | D(string)
  | U(string);

[@deriving (show({with_path: false}), yojson)]
type sys =
  | Mac
  | PC;

[@deriving (show({with_path: false}), yojson)]
type held =
  | Down
  | Up;

[@deriving (show({with_path: false}), yojson)]
type t = {
  key,
  code: string,
  sys,
  shift: held,
  meta: held,
  ctrl: held,
  alt: held,
  /* Tag of the DOM element the keydown originated from, e.g. "INPUT",
     "TEXTAREA", "SELECT", "BODY". Listeners use this to skip handling
     when the user is typing into a form element. */
  target_tag: option(string),
  target_id: option(string),
};

let get_key = evt =>
  Js.to_string(Js.Optdef.get(evt##.key, () => failwith("JsUtil.get_key")));

let ctrl_held = evt => Js.to_bool(evt##.ctrlKey);
let shift_held = evt => Js.to_bool(evt##.shiftKey);
let alt_held = evt => Js.to_bool(evt##.altKey);
let meta_held = evt => Js.to_bool(evt##.metaKey);

let key_of = (dir: dir, evt): key => {
  let key = get_key(evt);
  switch (dir) {
  | KeyUp => U(key)
  | KeyDown => D(key)
  };
};

let to_held: bool => held = b => b ? Down : Up;

let get_code = evt =>
  Js.to_string(Js.Optdef.get(evt##.code, () => Js.string("")));

let get_target_tag_and_id = (evt): (option(string), option(string)) =>
  switch (Js.Opt.to_option(evt##.target)) {
  | None => (None, None)
  | Some(el) =>
    let el = Js.Unsafe.coerce(el);
    let tag = Js.to_string(el##.tagName);
    let id =
      Js.Optdef.case(el##.id, () => None, s => Some(Js.to_string(s)));
    (Some(tag), id);
  };

let mk = (dir, evt): t => {
  let (target_tag, target_id) = get_target_tag_and_id(evt);
  {
    key: key_of(dir, evt),
    code: get_code(evt),
    sys: Os.is_mac^ ? Mac : PC,
    shift: to_held(shift_held(evt)),
    meta: to_held(meta_held(evt)),
    ctrl: to_held(ctrl_held(evt)),
    alt: to_held(alt_held(evt)),
    target_tag,
    target_id,
  };
};

let modifier_string = (h: held, m): string => h == Down ? " + " ++ m : "";

let modifiers_string = (key: t): string =>
  modifier_string(key.shift, "SHIFT")
  ++ modifier_string(key.meta, "META")
  ++ modifier_string(key.ctrl, "CTRL")
  ++ modifier_string(key.alt, "ALT");

let key_dir_string = (key: t): string =>
  switch (key.key) {
  | U(key) => "(UP): " ++ key
  | D(key) => "(DN): " ++ key
  };

let to_string = (key: t): string =>
  "KEY" ++ key_dir_string(key) ++ modifiers_string(key);

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
