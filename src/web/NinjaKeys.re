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

let update_shortcut_hotkey = (id, hotkey: string): unit => {
  let data = Js.Unsafe.get(elem(), "data");

  // Map over the array data and if the id matches, update the hotkey
  let new_data =
    Array.map(
      item => {
        let item_id = Js.Unsafe.get(item, "id") |> Js.to_string;
        if (item_id == id) {
          Js.Unsafe.set(item, "hotkey", Js.Optdef.option(Some(hotkey)));
        };
        item;
      },
      data |> Js.to_array,
    );
  Js.Unsafe.set(elem(), "data", Js.array(new_data));
};
