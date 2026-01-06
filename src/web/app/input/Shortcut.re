open Js_of_ocaml;

type t = {
  update_action: option(Page.Update.t),
  hotkey: option(string),
  label: string,
  mdIcon: option(string),
  section: option(string),
};

let mk_shortcut = (~hotkey=?, ~mdIcon=?, ~section=?, label, update_action): t => {
  {
    update_action: Some(update_action),
    hotkey,
    label,
    mdIcon,
    section,
  };
};

let instructor_shortcuts: list(t) = [
  mk_shortcut(
    ~mdIcon="download",
    ~section="Export",
    "Export Exercise Module",
    Editors(Exercises(ExportModule)) // TODO Would we rather skip contextual stuff for now or include it and have it fail
  ),
  mk_shortcut(
    ~mdIcon="download",
    ~section="Export",
    "Export Transitionary Exercise Module",
    Editors(Exercises(ExportTransitionary)) // TODO Would we rather skip contextual stuff for now or include it and have it fail
  ),
  mk_shortcut(
    ~mdIcon="download",
    ~section="Export",
    "Export Grading Exercise Module",
    Editors(Exercises(ExportGrading)) // TODO Would we rather skip contextual stuff for now or include it and have it fail
  ),
];

// List of shortcuts configured to show up in the command palette and have hotkey support
let shortcuts = (sys: Util.Key.sys): list(t) =>
  [
    mk_shortcut(
      ~mdIcon="undo",
      ~hotkey=Keyboard.meta(sys) ++ "+z",
      "Undo",
      Globals(Undo),
    ),
    mk_shortcut(
      ~hotkey=Keyboard.meta(sys) ++ "+shift+z",
      ~mdIcon="redo",
      "Redo",
      Globals(Redo),
    ),
    mk_shortcut(
      ~mdIcon="download",
      ~section="Export",
      "Export Scratch Slide",
      Editors(Scratch(Export)),
    ),
    mk_shortcut(
      "Add New Buffer",
      ~mdIcon="add",
      ~section="Buffers",
      Editors(Scratch(AddSlide)),
    ),
    mk_shortcut(
      "Rename Current Buffer",
      ~mdIcon="edit",
      ~section="Buffers",
      Editors(Scratch(RenameSlide)),
    ),
    mk_shortcut(
      ~mdIcon="delete",
      ~section="Buffers",
      "Delete Current Buffer",
      Editors(Scratch(DeleteSlide)),
    ),
  ]
  @ (if (ExerciseSettings.show_instructor) {instructor_shortcuts} else {[]});

let from_shortcut =
    (schedule_action: Page.Update.t => unit, shortcut: t)
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
         | Some(update) => schedule_action(update)
         | None =>
           print_endline("Could not find action for " ++ shortcut.label)
         };
       }
   }
  ];
};

let options = (schedule_action: Page.Update.t => unit) => {
  Array.of_list(
    List.map(
      from_shortcut(schedule_action),
      shortcuts(Util.Os.is_mac^ ? Util.Key.Mac : PC),
    ),
  );
};
