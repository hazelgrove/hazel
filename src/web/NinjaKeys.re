open Js_of_ocaml;
open Util_web;

/*
 Configuration of the command palette using the https://github.com/ssleptsov/ninja-keys web component.
 */

let elem = () => JsUtil.get_elem_by_id("ninja-keys");

let open_command_palette = (): unit => {
  Js.Unsafe.meth_call(
    elem(),
    "open",
    [||] // Can't use ##.open because open is a reserved keyword
  );
};

let of_contextual_action =
    (action: ContextualAction.t)
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
     val id = action.label;
     val title = action.label;
     val mdIcon = Js.Optdef.option(action.mdIcon);
     val hotkey = Js.Optdef.option(action.hotkey);
     val section = Js.Optdef.option(action.section);
     val handler =
       () => {
         switch (action.update_action) {
         | Some(effect) =>
           Virtual_dom.Vdom.Effect.Expert.handle_non_dom_event_exn(effect)
         | None => print_endline("Could not find action for " ++ action.label)
         };
       }
   }
  ];
};

let initialize = (actions: list(ContextualAction.t)) => {
  let opts = Array.of_list(List.map(of_contextual_action, actions));
  Js.Unsafe.set(elem(), "data", Js.array(opts));
};
