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

let of_contextual_action =
    (
      ~schedule_effect: Ui_effect.t(unit) => unit,
      shortcut: Haz3lcore.ContextualAction.t,
    )
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
         switch (shortcut.update_action) {
         | Some(update) => schedule_effect(update)
         | None =>
           print_endline("Could not find action for " ++ shortcut.label)
         };
       }
   }
  ];
};
let options =
    (~schedule_effect: Ui_effect.t(unit) => unit, contextual_actions) => {
  Array.of_list(
    List.map(of_contextual_action(~schedule_effect), contextual_actions),
  );
};
