open Js_of_ocaml;

/*
 Configuration of the command palette using the https://github.com/ssleptsov/ninja-keys web component.
 */

let go_to_scratch_slide: {
  ..
  "children":
    Js.readonly_prop(
      Js.optdef(
        Js.t(
          Js.js_array({
            ..
            "id": Js.readonly_prop(string),
            "title": Js.readonly_prop(string),
          }),
        ),
      ),
    ),
  "handler": Js.readonly_prop(Js.optdef('a)),
  "hotkey": Js.readonly_prop(Js.optdef('b)),
  "id": Js.readonly_prop(string),
  "mdIcon": Js.readonly_prop(Js.optdef('c)),
  "title": Js.readonly_prop(string),
} = [%js
  {
    as _;
    val id = "Go to Scratch Slide";
    val title = "Go to Scratch Slide";
    val mdIcon = Js.undefined;
    val hotkey = Js.undefined;
    val handler = Js.undefined;
    val children =
      Js.Optdef.return(
        Js.array([|
          [%js
            {
              val id = "Go to Scratch Slide 1";
              val title = "Go to Scratch Slide 1"
            }
          ],
        |]),
      )
  }
];

let from_shortcut =
    (schedule_action: UpdateAction.t => unit, shortcut: Keyboard.shortcut)
    : {
        ..
        "children": Js.readonly_prop(Js.optdef('a)),
        "handler": Js.readonly_prop(Js.optdef(unit => unit)),
        "hotkey": Js.readonly_prop(Js.optdef(string)),
        "id": Js.readonly_prop(string),
        "mdIcon": Js.readonly_prop(Js.optdef(string)),
        "title": Js.readonly_prop(string),
      } => {
  [%js
   {
     as _;
     val id = shortcut.label;
     val title = shortcut.label;
     val mdIcon = Js.Optdef.option(shortcut.mdIcon);
     val hotkey = Js.Optdef.option(shortcut.hotkey);
     val children = Js.undefined;
     val handler =
       Js.Optdef.return(() => {
         let foo = shortcut.update_action;
         schedule_action(foo);
         //  switch (foo) {
         //  | Some(update) => schedule_action(update)
         //  | None =>
         //    print_endline("Could not find action for " ++ shortcut.label)
         //  };
       })
   }];
};

let options = (schedule_action: UpdateAction.t => unit) => {
  Array.of_list(
    List.map(
      from_shortcut(schedule_action),
      Keyboard.shortcuts(Os.is_mac^ ? Mac : PC),
    )
    @ [go_to_scratch_slide],
  );
};

let elem = () => JsUtil.get_elem_by_id("ninja-keys");

let initialize = opts => Js.Unsafe.set(elem(), "data", Js.array(opts));
