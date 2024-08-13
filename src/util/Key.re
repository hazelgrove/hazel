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
  sys,
  shift: held,
  meta: held,
  ctrl: held,
  alt: held,
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

let mk = (dir, evt): t => {
  key: key_of(dir, evt),
  sys: Os.is_mac^ ? Mac : PC,
  shift: to_held(shift_held(evt)),
  meta: to_held(meta_held(evt)),
  ctrl: to_held(ctrl_held(evt)),
  alt: to_held(alt_held(evt)),
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
