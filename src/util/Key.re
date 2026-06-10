open Ppx_yojson_conv_lib.Yojson_conv;

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
};

let to_held: bool => held = b => b ? Down : Up;

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
