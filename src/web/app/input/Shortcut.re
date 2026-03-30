type t = {
  update_action: Page.Update.t,
  hotkey: option(string),
  label: string,
  mdIcon: option(string),
  section: option(string),
};

let mk_shortcut = (~hotkey=?, ~mdIcon=?, ~section=?, label, update_action): t => {
  update_action,
  hotkey,
  label,
  mdIcon,
  section,
};

let to_contextual_action =
    (~inject: Page.Update.t => Virtual_dom.Vdom.Effect.t(unit), shortcut: t)
    : ContextualAction.t => {
  {
    update_action: Some(inject(shortcut.update_action)),
    hotkey: shortcut.hotkey,
    label: shortcut.label,
    mdIcon: shortcut.mdIcon,
    section: shortcut.section,
  };
};

let instructor_shortcuts: list(t) = [
  mk_shortcut(
    ~mdIcon="download",
    ~section="Export",
    "Export Exercise Module",
    Editors(Exercises(ExportModule)),
  ),
  mk_shortcut(
    ~mdIcon="download",
    ~section="Export",
    "Export Transitionary Exercise Module",
    Editors(Exercises(ExportTransitionary)),
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
      Globals(ActiveEditor(Move(Goal(BindingSiteOfIndicatedVar)))),
    ),
    mk_shortcut(
      ~hotkey="shift+tab",
      ~mdIcon="arrow_upward",
      ~section="Navigation",
      "Go to Previous Problem",
      Globals(ActiveEditor(Move(Goal(NextProblem(Left))))),
    ),
    mk_shortcut(
      ~mdIcon="arrow_downward",
      ~section="Navigation",
      "Go to Next Problem",
      Globals(ActiveEditor(Move(Goal(NextProblem(Right))))),
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
      ~mdIcon="select_all",
      ~hotkey=Keyboard.meta(sys) ++ "+a",
      ~section="Selection",
      "Select All",
      Globals(ActiveEditor(Select(All))),
    ),
    mk_shortcut(
      ~mdIcon="flip_horizontal",
      ~section="Selection",
      "Toggle Selection Focus",
      Globals(ActiveEditor(Select(ToggleFocus))),
    ),
    mk_shortcut(
      ~mdIcon="border_left",
      ~section="Selection",
      ~hotkey=Keyboard.meta(sys) ++ "+alt+shift+left",
      "Set Selection Focus Left",
      Globals(ActiveEditor(Select(SetFocus(Left)))),
    ),
    mk_shortcut(
      ~mdIcon="border_right",
      ~section="Selection",
      ~hotkey=Keyboard.meta(sys) ++ "+alt+shift+right",
      "Set Selection Focus Right",
      Globals(ActiveEditor(Select(SetFocus(Right)))),
    ),
    mk_shortcut(
      ~mdIcon="chevron_left",
      ~section="Selection",
      ~hotkey="alt+shift+left",
      "Extend Selection Left by Token",
      Globals(ActiveEditor(Select(Resize(Local(Left, ByToken))))),
    ),
    mk_shortcut(
      ~mdIcon="chevron_right",
      ~section="Selection",
      ~hotkey="alt+shift+right",
      "Extend Selection Right by Token",
      Globals(ActiveEditor(Select(Resize(Local(Right, ByToken))))),
    ),
    mk_shortcut(
      ~hotkey="alt+f",
      ~mdIcon="camera",
      ~section="Projection",
      "Fold",
      Globals(ActiveEditor(Project(SetIndicated(Specific(Fold))))),
    ),
    mk_shortcut(
      ~hotkey=Keyboard.meta(sys) ++ "+e",
      ~mdIcon="camera",
      ~section="Projection",
      "Probe",
      Globals(ActiveEditor(Probe(ToggleManual))),
    ),
    mk_shortcut(
      ~hotkey="alt+t",
      ~mdIcon="camera",
      ~section="Projection",
      "Statics",
      Globals(ActiveEditor(Probe(ToggleStatics))),
    ),
    mk_shortcut(
      ~hotkey=Keyboard.meta(sys) ++ "+p",
      ~mdIcon="science",
      ~section="Projection",
      "Toggle Auto Probe",
      Globals(Set(AutoprobeMode)),
    ),
    mk_shortcut(
      ~hotkey="alt+l",
      ~mdIcon="camera",
      ~section="Projection",
      "Livelit",
      Globals(ActiveEditor(Project(SetIndicated(ChooseLivelit)))),
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
      "Toggle Dynamics",
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
      "Toggle Show Ascription Steps",
      Globals(Set(Evaluation(ShowAscriptionSteps))),
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
      "Toggle Show Sidebar",
      Globals(Set(Sidebar(ToggleShow))),
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
      Globals(ActiveEditor(Buffer(Set(TyDi)))),
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
      "Encode Scratch Slide in URL",
      Editors(Scratch(Encode)),
    ),
    mk_shortcut(
      ~mdIcon="download",
      ~section="Export",
      "Export For Init",
      Globals(ExportForInit),
    ),
    mk_shortcut(
      ~mdIcon="download",
      ~section="Export",
      "Export Submission",
      Editors(Exercises(ExportSubmission)),
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
    mk_shortcut(
      ~mdIcon="bolt",
      ~section="Refactoring",
      ~hotkey=Keyboard.meta(sys) ++ "+i",
      "Introduce",
      Globals(ActiveEditor(Introduce)),
    ),
    mk_shortcut(
      "Add New Buffer",
      ~hotkey="alt+n",
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

let contextual_actions =
    (
      ~inject: Page.Update.t => Virtual_dom.Vdom.Effect.t(unit),
      sys: Util.Key.sys,
    )
    : list(ContextualAction.t) => {
  List.map(to_contextual_action(~inject), shortcuts(sys));
};
