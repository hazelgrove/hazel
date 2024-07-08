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
     val hotkey = Js.Opt.return(shortcut.hotkey);
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
  let shortcuts =
    Array.of_list(
      List.map(from_shortcut(schedule_action), Keyboard.shortcuts),
    );
  Array.append(
    shortcuts,
    [|
      [%js
        {
          val id = "Proceed To Next Hole";
          val title = "Proceed To Next Hole";
          val mdIcon = Js.Opt.return("arrow_forward");
          val hotkey = Js.Opt.empty;
          val handler =
            () => {
              schedule_action(PerformAction(MoveToNextHole(Right)));
            }
        }
      ],
      [%js
        {
          val id = "Undo";
          val title = "Undo";
          val mdIcon = Js.Opt.return("undo");
          val hotkey = Js.Opt.empty;
          val handler =
            () => {
              schedule_action(Undo);
            }
        }
      ],
      [%js
        {
          val id = "Redo";
          val title = "Redo";
          val mdIcon = Js.Opt.return("redo");
          val hotkey = Js.Opt.empty;
          val handler =
            () => {
              schedule_action(Redo);
            }
        }
      ],
      [%js
        {
          val id = "Reparse Current Editor";
          val title = "Reparse Current Editor";
          val mdIcon = Js.Opt.return("refresh");
          val hotkey = Js.Opt.empty;
          val handler =
            () => {
              schedule_action(ReparseCurrentEditor);
            }
        }
      ],
    |],
  );
};

let elem = () => JsUtil.get_elem_by_id("ninja-keys");

let initialize = opts => Js.Unsafe.set(elem(), "data", Js.array(opts));
