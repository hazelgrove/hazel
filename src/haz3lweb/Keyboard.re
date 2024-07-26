open Haz3lcore;

let is_digit = s => Re.Str.(string_match(regexp("^[0-9]$"), s, 0));
let is_f_key = s => Re.Str.(string_match(regexp("^F[0-9][0-9]*$"), s, 0));

type shortcut = {
  update_action: UpdateAction.t,
  hotkey: option(string),
  label: string,
  mdIcon: option(string),
  parent: option(string),
  children: list(string),
};

let meta = (sys: Key.sys): string => {
  switch (sys) {
  | Mac => "cmd"
  | PC => "ctrl"
  };
};

let mk_shortcut =
    (~hotkey=?, ~mdIcon=?, ~parent=?, ~children=[], label, update_action)
    : shortcut => {
  {update_action, hotkey, label, mdIcon, parent, children};
};

// List of shortcuts configured to show up in the command palette and have hotkey support
let shortcuts = (sys: Key.sys): list(shortcut) => [
  mk_shortcut(
    ~hotkey="F12",
    ~mdIcon="arrow_forward",
    "Go to Definition",
    PerformAction(Jump(BindingSiteOfIndicatedVar)),
  ),
  mk_shortcut(
    ~hotkey="shift+tab",
    ~mdIcon="swipe_left_alt",
    "Go to Definition",
    MoveToNextHole(Left),
  ),
  mk_shortcut(
    ~mdIcon="swipe_right_alt",
    "Go To Next Hole",
    MoveToNextHole(Right),
    // Tab is overloaded so not setting it here
  ),
  mk_shortcut(~mdIcon="undo", ~hotkey=meta(sys) ++ "+z", "Undo", Undo),
  mk_shortcut(~hotkey=meta(sys) ++ "+shift+z", ~mdIcon="redo", "Redo", Redo),
  mk_shortcut(
    ~hotkey=meta(sys) ++ "+d",
    ~mdIcon="select_all",
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
    "Select All",
    PerformAction(Select(All)),
  ),
  mk_shortcut(
    ~hotkey=meta(sys) ++ "+/",
    ~mdIcon="assistant",
    "TyDi Assistant",
    Assistant(Prompt(TyDi)) // I haven't figured out how to trigger this in the editor
  ),
  mk_shortcut("Export Scratch Slide", ExportScratchSlide),
  mk_shortcut(
    // ctrl+k conflicts with the command palette
    ~mdIcon="refresh",
    "Reparse Current Editor",
    ReparseCurrentEditor,
  ),
  mk_shortcut(~hotkey="F7", "Run Benchmark", Benchmark(Start)),
];

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
    | "/" => Some(Assistant(Prompt(TyDi)))
    | "ArrowLeft" => now(Move(Extreme(Left(ByToken))))
    | "ArrowRight" => now(Move(Extreme(Right(ByToken))))
    | "ArrowUp" => now(Move(Extreme(Up)))
    | "ArrowDown" => now(Move(Extreme(Down)))
    | _ => None
    }
  | {key: D(key), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "/" => Some(Assistant(Prompt(TyDi)))
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
  | {key: D(key), sys, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    switch (sys, key) {
    | (_, "ArrowLeft") => now(MoveToBackpackTarget(Left(ByToken)))
    | (_, "ArrowRight") => now(MoveToBackpackTarget(Right(ByToken)))
    | (_, "Alt") => Some(SetMeta(ShowBackpackTargets(true)))
    | (_, "ArrowUp") => now(MoveToBackpackTarget(Up))
    | (_, "ArrowDown") => now(MoveToBackpackTarget(Down))
    | _ => None
    }
  | _ => None
  };
};
