open Js_of_ocaml;

/*
 Configuration of the command palette using the https://github.com/ssleptsov/ninja-keys web component.
 */

let from_shortcut =
    (schedule_action: UpdateAction.t => unit, shortcut: Keyboard.shortcut)
    : {
        .
        "handler": Js.readonly_prop(unit => unit),
        "id": Js.readonly_prop(string),
        "mdIcon": Js.readonly_prop(Js.optdef(string)),
        "hotkey": Js.readonly_prop(Js.optdef(string)),
        "title": Js.readonly_prop(string),
        "section": Js.readonly_prop(Js.optdef(string)),
      } => {
  [%js
   {
     val id = shortcut.label;
     val title = shortcut.label;
     val mdIcon = Js.Optdef.option(shortcut.mdIcon);
     val hotkey = Js.Optdef.option(shortcut.hotkey);
     val section = Js.Optdef.option(shortcut.section);
     val handler =
       () => {
         let foo = shortcut.update_action;
         switch (foo) {
         | Some(update) => schedule_action(update)
         | None =>
           print_endline("Could not find action for " ++ shortcut.label)
         };
       }
   }];
};

let options = (schedule_action: UpdateAction.t => unit) => {
  Array.of_list(
    List.map(
      from_shortcut(schedule_action),
      Keyboard.shortcuts(Os.is_mac^ ? Mac : PC),
    ),
  );
};

let elem = () => JsUtil.get_elem_by_id("ninja-keys");

let initialize = opts => Js.Unsafe.set(elem(), "data", Js.array(opts));

let open_command_palette = (): unit => {
  Js.Unsafe.meth_call(
    elem(),
    "open",
    [||] // Can't use ##.open because open is a reserved keyword
  );
};
