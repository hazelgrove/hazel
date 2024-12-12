open Js_of_ocaml;

type t = {
  update_action: option(Page.Update.t),
  hotkey: option(string),
  label: string,
  mdIcon: option(string),
  section: option(string),
};

let mk_shortcut = (~hotkey=?, ~mdIcon=?, ~section=?, label, update_action): t => {
  {update_action: Some(update_action), hotkey, label, mdIcon, section};
};

let instructor_shortcuts: list(t) = [
  mk_shortcut(
    ~mdIcon="download",
    ~section="Export",
    "Export All Persistent Data",
    Globals(ExportPersistentData),
  ),
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
      ~hotkey="F12",
      ~mdIcon="arrow_forward",
      ~section="Navigation",
      "Go to Definition",
      Globals(ActiveEditor(Jump(BindingSiteOfIndicatedVar))),
    ),
    mk_shortcut(
      ~hotkey="shift+tab",
      ~mdIcon="swipe_left_alt",
      ~section="Navigation",
      "Go to Previous Hole",
      Globals(ActiveEditor(Move(Goal(Piece(Grout, Left))))),
    ),
    mk_shortcut(
      ~mdIcon="swipe_right_alt",
      ~section="Navigation",
      "Go To Next Hole",
      Globals(ActiveEditor(Move(Goal(Piece(Grout, Right))))),
      // Tab is overloaded so not setting it here
    ),
    mk_shortcut(
      ~hotkey=Keyboard.meta(sys) ++ "+d",
      ~mdIcon="select_all",
      ~section="Selection",
      "Select current term",
      Globals(ActiveEditor(Select(Term(Current)))),
    ),
    mk_shortcut(
      ~hotkey=Keyboard.meta(sys) ++ "+p",
      ~mdIcon="backpack",
      "Pick up selected term",
      Globals(ActiveEditor(Pick_up)),
    ),
    mk_shortcut(
      ~mdIcon="select_all",
      ~hotkey=Keyboard.meta(sys) ++ "+a",
      ~section="Selection",
      "Select All",
      Globals(ActiveEditor(Select(All))),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Statics",
      Globals(Set(Statics)),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Completion",
      Globals(Set(Assist)),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Whitespace",
      Globals(Set(SecondaryIcons)),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Print Benchmarks",
      Globals(Set(Benchmark)),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Toggle Dynamics",
      Globals(Set(Dynamics)),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Elaboration",
      Globals(Set(Elaborate)),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Function Bodies",
      Globals(Set(Evaluation(ShowFnBodies))),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Case Clauses",
      Globals(Set(Evaluation(ShowCaseClauses))),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show fixpoints",
      Globals(Set(Evaluation(ShowFixpoints))),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Casts",
      Globals(Set(Evaluation(ShowCasts))),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Lookup Steps",
      Globals(Set(Evaluation(ShowLookups))),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Stepper Filters",
      Globals(Set(Evaluation(ShowFilters))),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Hidden Steps",
      Globals(Set(Evaluation(ShowHiddenSteps))),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Docs Sidebar",
      Globals(Set(ExplainThis(ToggleShow))),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Docs Feedback",
      Globals(Set(ExplainThis(ToggleShowFeedback))),
    ),
    mk_shortcut(
      ~hotkey=Keyboard.meta(sys) ++ "+/",
      ~mdIcon="assistant",
      "TyDi Assistant",
      Globals(ActiveEditor(Buffer(Set(TyDi)))) // I haven't figured out how to trigger this in the editor
    ),
    mk_shortcut(
      ~mdIcon="download",
      ~section="Export",
      "Export Scratch Slide",
      Editors(Scratch(Export)),
    ),
    mk_shortcut(
      ~mdIcon="download",
      ~section="Export",
      "Export Submission",
      Editors(Exercises(ExportSubmission)) // TODO Would we rather skip contextual stuff for now or include it and have it fail
    ),
    mk_shortcut(
      // ctrl+k conflicts with the command palette
      ~section="Diagnostics",
      ~mdIcon="refresh",
      "Reparse Current Editor",
      Globals(ActiveEditor(Reparse)),
    ),
    mk_shortcut(
      ~mdIcon="timer",
      ~section="Diagnostics",
      ~hotkey="F7",
      "Run Benchmark",
      Benchmark(Start),
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
         let foo = shortcut.update_action;
         switch (foo) {
         | Some(update) => schedule_action(update)
         | None =>
           print_endline("Could not find action for " ++ shortcut.label)
         };
       }
   }];
};

let options = (schedule_action: Page.Update.t => unit) => {
  Array.of_list(
    List.map(
      from_shortcut(schedule_action),
      shortcuts(Util.Os.is_mac^ ? Util.Key.Mac : PC),
    ),
  );
};
