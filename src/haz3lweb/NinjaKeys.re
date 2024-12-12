open Js_of_ocaml;
open Util;

/*
 Configuration of the command palette using the https://github.com/ssleptsov/ninja-keys web component.
 */

let elem = () => JsUtil.get_elem_by_id("ninja-keys");

let initialize = opts => Js.Unsafe.set(elem(), "data", Js.array(opts));

let open_command_palette = (): unit => {
  Js.Unsafe.meth_call(
    elem(),
    "open",
    [||] // Can't use ##.open because open is a reserved keyword
  );
};
