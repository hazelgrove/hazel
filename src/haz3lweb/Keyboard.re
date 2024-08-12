open Haz3lcore;
open Util;

let is_digit = s => StringUtil.(match(regexp("^[0-9]$"), s));
let is_f_key = s => StringUtil.(match(regexp("^F[0-9][0-9]*$"), s));

type shortcut = {
  update_action: option(UpdateAction.t),
  hotkey: option(string),
  label: string,
  mdIcon: option(string),
  section: option(string),
};

let meta = (sys: Key.sys): string => {
  switch (sys) {
  | Mac => "cmd"
  | PC => "ctrl"
  };
};

let mk_shortcut =
    (~hotkey=?, ~mdIcon=?, ~section=?, label, update_action): shortcut => {
  {update_action: Some(update_action), hotkey, label, mdIcon, section};
};

let instructor_shortcuts: list(shortcut) = [
  mk_shortcut(
    ~mdIcon="download",
    ~section="Export",
    "Export All Persistent Data",
    Export(ExportPersistentData),
  ),
  mk_shortcut(
    ~mdIcon="download",
    ~section="Export",
    "Export Exercise Module",
    Export(ExerciseModule) // TODO Would we rather skip contextual stuff for now or include it and have it fail
  ),
  mk_shortcut(
    ~mdIcon="download",
    ~section="Export",
    "Export Transitionary Exercise Module",
    Export(TransitionaryExerciseModule) // TODO Would we rather skip contextual stuff for now or include it and have it fail
  ),
  mk_shortcut(
    ~mdIcon="download",
    ~section="Export",
    "Export Grading Exercise Module",
    Export(GradingExerciseModule) // TODO Would we rather skip contextual stuff for now or include it and have it fail
  ),
];

// List of shortcuts configured to show up in the command palette and have hotkey support
let shortcuts = (sys: Key.sys): list(shortcut) =>
  [
    mk_shortcut(~mdIcon="undo", ~hotkey=meta(sys) ++ "+z", "Undo", Undo),
    mk_shortcut(
      ~hotkey=meta(sys) ++ "+shift+z",
      ~mdIcon="redo",
      "Redo",
      Redo,
    ),
    mk_shortcut(
      ~hotkey="F12",
      ~mdIcon="arrow_forward",
      ~section="Navigation",
      "Go to Definition",
      PerformAction(Jump(BindingSiteOfIndicatedVar)),
    ),
    mk_shortcut(
      ~hotkey="shift+tab",
      ~mdIcon="swipe_left_alt",
      ~section="Navigation",
      "Go to Previous Hole",
      PerformAction(Move(Goal(Piece(Grout, Left)))),
    ),
    mk_shortcut(
      ~mdIcon="swipe_right_alt",
      ~section="Navigation",
      "Go To Next Hole",
      PerformAction(Move(Goal(Piece(Grout, Right)))),
      // Tab is overloaded so not setting it here
    ),
    mk_shortcut(
      ~hotkey=meta(sys) ++ "+d",
      ~mdIcon="select_all",
      ~section="Selection",
      "Select current term",
      PerformAction(Select(Term(Current))),
    ),
    mk_shortcut(
      ~hotkey=meta(sys) ++ "+p",
      ~mdIcon="backpack",
      "Pick up selected term",
      PerformAction(Pick_up),
    ),
    mk_shortcut(
      ~mdIcon="select_all",
      ~hotkey=meta(sys) ++ "+a",
      ~section="Selection",
      "Select All",
      PerformAction(Select(All)),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Statics",
      UpdateAction.Set(Statics),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Completion",
      UpdateAction.Set(Assist),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Whitespace",
      UpdateAction.Set(SecondaryIcons),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Print Benchmarks",
      UpdateAction.Set(Benchmark),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Toggle Dynamics",
      UpdateAction.Set(Dynamics),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Elaboration",
      UpdateAction.Set(Elaborate),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Function Bodies",
      UpdateAction.Set(Evaluation(ShowFnBodies)),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Case Clauses",
      UpdateAction.Set(Evaluation(ShowCaseClauses)),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show fixpoints",
      UpdateAction.Set(Evaluation(ShowFixpoints)),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Casts",
      UpdateAction.Set(Evaluation(ShowCasts)),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Lookup Steps",
      UpdateAction.Set(Evaluation(ShowLookups)),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Stepper Filters",
      UpdateAction.Set(Evaluation(ShowFilters)),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Hidden Steps",
      UpdateAction.Set(Evaluation(ShowHiddenSteps)),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Docs Sidebar",
      UpdateAction.Set(ExplainThis(ToggleShow)),
    ),
    mk_shortcut(
      ~section="Settings",
      ~mdIcon="tune",
      "Toggle Show Docs Feedback",
      UpdateAction.Set(ExplainThis(ToggleShowFeedback)),
    ),
    mk_shortcut(
      ~hotkey=meta(sys) ++ "+/",
      ~mdIcon="assistant",
      "TyDi Assistant",
      PerformAction(Buffer(Set(TyDi))) // I haven't figured out how to trigger this in the editor
    ),
    mk_shortcut(
      ~mdIcon="download",
      ~section="Export",
      "Export Scratch Slide",
      Export(ExportScratchSlide),
    ),
    mk_shortcut(
      ~mdIcon="download",
      ~section="Export",
      "Export Submission",
      Export(Submission) // TODO Would we rather skip contextual stuff for now or include it and have it fail
    ),
    mk_shortcut(
      // ctrl+k conflicts with the command palette
      ~section="Diagnostics",
      ~mdIcon="refresh",
      "Reparse Current Editor",
      PerformAction(Reparse),
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

let handle_key_event = (k: Key.t): option(Update.t) => {
  let now = (a: Action.t): option(UpdateAction.t) =>
    Some(PerformAction(a));
  switch (k) {
  | {key: U(key), _} =>
    /* Keu-UPpEvents:
       NOTE: Remember that since there is a keyup for every
       keydown, making an update here may trigger an entire
       extra redraw, contingent on model.cutoff */
    switch (key) {
    | "Alt" => Some(SetMeta(ShowBackpackTargets(false)))
    | _ => None
    }
  | {key: D(key), sys: _, shift: Down, meta: Up, ctrl: Up, alt: Up}
      when is_f_key(key) =>
    switch (key) {
    | _ => Some(DebugConsole(key))
    }
  | {key: D(key), sys: _, shift, meta: Up, ctrl: Up, alt: Up} =>
    switch (shift, key) {
    | (Up, "ArrowLeft") => now(Move(Local(Left(ByChar))))
    | (Up, "ArrowRight") => now(Move(Local(Right(ByChar))))
    | (Up, "ArrowUp") => now(Move(Local(Up)))
    | (Up, "ArrowDown") => now(Move(Local(Down)))
    | (Up, "Home") => now(Move(Extreme(Left(ByToken))))
    | (Up, "End") => now(Move(Extreme(Right(ByToken))))
    | (Up, "Backspace") => now(Destruct(Left))
    | (Up, "Delete") => now(Destruct(Right))
    | (Up, "Escape") => now(Unselect(None))
    | (Up, "Tab") => Some(TAB)
    | (Up, "F12") => now(Jump(BindingSiteOfIndicatedVar))
    | (Down, "Tab") => now(Move(Goal(Piece(Grout, Left))))
    | (Down, "ArrowLeft") => now(Select(Resize(Local(Left(ByToken)))))
    | (Down, "ArrowRight") => now(Select(Resize(Local(Right(ByToken)))))
    | (Down, "ArrowUp") => now(Select(Resize(Local(Up))))
    | (Down, "ArrowDown") => now(Select(Resize(Local(Down))))
    | (Down, "Home") => now(Select(Resize(Extreme(Left(ByToken)))))
    | (Down, "End") => now(Select(Resize(Extreme(Right(ByToken)))))
    | (_, "Enter") => now(Insert(Form.linebreak))
    | _ when String.length(key) == 1 =>
      /* Note: length==1 prevent specials like
       * SHIFT from being captured here */
      now(Insert(key))
    | _ => None
    }
  | {key: D(key), sys: Mac, shift: Down, meta: Down, ctrl: Up, alt: Up} =>
    switch (key) {
    | "ArrowLeft" => now(Select(Resize(Extreme(Left(ByToken)))))
    | "ArrowRight" => now(Select(Resize(Extreme(Right(ByToken)))))
    | "ArrowUp" => now(Select(Resize(Extreme(Up))))
    | "ArrowDown" => now(Select(Resize(Extreme(Down))))
    | _ => None
    }
  | {key: D(key), sys: PC, shift: Down, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "ArrowLeft" => now(Select(Resize(Local(Left(ByToken)))))
    | "ArrowRight" => now(Select(Resize(Local(Right(ByToken)))))
    | "ArrowUp" => now(Select(Resize(Local(Up))))
    | "ArrowDown" => now(Select(Resize(Local(Down))))
    | "Home" => now(Select(Resize(Extreme(Up))))
    | "End" => now(Select(Resize(Extreme(Down))))
    | _ => None
    }
  | {key: D(key), sys: Mac, shift: Up, meta: Down, ctrl: Up, alt: Up} =>
    switch (key) {
    | "z" => Some(Undo)
    | "d" => now(Select(Term(Current)))
    | "p" => Some(PerformAction(Pick_up))
    | "a" => now(Select(All))
    | "/" => Some(PerformAction(Buffer(Set(TyDi))))
    | "ArrowLeft" => now(Move(Extreme(Left(ByToken))))
    | "ArrowRight" => now(Move(Extreme(Right(ByToken))))
    | "ArrowUp" => now(Move(Extreme(Up)))
    | "ArrowDown" => now(Move(Extreme(Down)))
    | _ => None
    }
  | {key: D(key), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "z" => Some(Undo)
    | "d" => now(Select(Term(Current)))
    | "p" => Some(PerformAction(Pick_up))
    | "a" => now(Select(All))
    | "/" => Some(PerformAction(Buffer(Set(TyDi))))
    | "ArrowLeft" => now(Move(Local(Left(ByToken))))
    | "ArrowRight" => now(Move(Local(Right(ByToken))))
    | "Home" => now(Move(Extreme(Up)))
    | "End" => now(Move(Extreme(Down)))
    | _ => None
    }
  | {key: D(key), sys: Mac, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "a" => now(Move(Extreme(Left(ByToken))))
    | "e" => now(Move(Extreme(Right(ByToken))))
    | _ => None
    }
  | {key: D("f"), sys: PC, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    Some(PerformAction(Project(ToggleIndicated(Fold))))
  | {key: D("ƒ"), sys: Mac, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    /* Curly ƒ is what holding option turns f into on Mac */
    Some(PerformAction(Project(ToggleIndicated(Fold))))
  | {key: D(key), sys: _, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    switch (key) {
    | "ArrowLeft" => now(MoveToBackpackTarget(Left(ByToken)))
    | "ArrowRight" => now(MoveToBackpackTarget(Right(ByToken)))
    | "Alt" => Some(SetMeta(ShowBackpackTargets(true)))
    | "ArrowUp" => now(MoveToBackpackTarget(Up))
    | "ArrowDown" => now(MoveToBackpackTarget(Down))
    | _ => None
    }
  | _ => None
  };
};
