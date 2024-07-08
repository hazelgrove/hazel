open Js_of_ocaml;

let from_shortcut =
    (schedule_action: UpdateAction.t => unit, shortcut: Keyboard.shortcut)
    : {
        .
        "handler": Js.readonly_prop(unit => unit),
        "id": Js.readonly_prop(string),
        "mdIcon": Js.readonly_prop(Js.opt(string)),
        "hotkey": Js.readonly_prop(Js.opt(string)),
        "title": Js.readonly_prop(string),
      } => {
  [%js
   {
     val id = shortcut.label;
     val title = shortcut.label;
     val mdIcon = Js.Opt.option(shortcut.mdIcon);
     val hotkey = Js.Opt.option(shortcut.hotkey);
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
    List.map(from_shortcut(schedule_action), Keyboard.shortcuts),
  );
};

let elem = () => JsUtil.get_elem_by_id("ninja-keys");

let initialize = opts => Js.Unsafe.set(elem(), "data", Js.array(opts));
