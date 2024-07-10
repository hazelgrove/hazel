open Haz3lcore;

let is_digit = s => Re.Str.(string_match(regexp("^[0-9]$"), s, 0));
let is_f_key = s => Re.Str.(string_match(regexp("^F[0-9][0-9]*$"), s, 0));

type shortcut = {
  update_action: option(UpdateAction.t),
  hotkey: option(string),
  label: string,
  mdIcon: option(string),
};

// Currently PC only. I need to figure out how to handle mac.
let shortcuts: list(shortcut) = [
  {
    update_action: Some(Benchmark(Start)),
    hotkey: Some("F7"),
    label: "Run Benchmark",
    mdIcon: None,
  },
  {
    update_action: Some(PerformAction(Jump(BindingSiteOfIndicatedVar))),
    hotkey: Some("F12"),
    label: "Go to Definition",
    mdIcon: Some("arrow_forward"),
  },
  {
    update_action: Some(Redo),
    hotkey: Some("ctrl+shift+z,cmd+shift+z"),
    label: "Redo",
    mdIcon: Some("redo"),
  },
  {
    update_action: Some(Undo),
    hotkey: Some("ctrl+z,cmd+z"),
    label: "Undo",
    mdIcon: Some("undo"),
  },
  {
    update_action: Some(MoveToNextHole(Left)),
    hotkey: Some("shift+tab"),
    label: "Go To Previous Hole",
    mdIcon: Some("swipe_left_alt"),
  },
  {
    update_action: Some(MoveToNextHole(Right)),
    hotkey: None, // Tab is overloaded so not setting it here
    label: "Go To Next Hole",
    mdIcon: Some("swipe_right_alt"),
  },
  {
    update_action: Some(ReparseCurrentEditor),
    hotkey: None, // ctrl+k conflicts with the command palette
    label: "Reparse Current Editor",
    mdIcon: Some("refresh"),
  },
  {
    update_action: Some(PerformAction(Select(Term(Current)))),
    hotkey: Some("ctrl+d,cmd+d"),
    label: "Select current term",
    mdIcon: Some("select_all"),
  },
  {
    update_action: Some(PerformAction(Pick_up)),
    hotkey: Some("ctrl+p,cmd+p"),
    label: "Pick up selected term",
    mdIcon: Some("backpack"),
  },
  {
    update_action: Some(PerformAction(Select(All))),
    hotkey: Some("ctrl+a,cmd+a"),
    label: "Select All",
    mdIcon: Some("select_all"),
  },
  {
    update_action: Some(Assistant(Prompt(TyDi))), // I haven't figured out how to trigger this in the editor
    hotkey: Some("ctrl+/,cmd+/"),
    label: "TyDi Assistant",
    mdIcon: Some("assistant"),
  },
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
    | _ when is_digit(key) => Some(SwitchScratchSlide(int_of_string(key)))
    | "ArrowLeft" => now(Move(Extreme(Left(ByToken))))
    | "ArrowRight" => now(Move(Extreme(Right(ByToken))))
    | "ArrowUp" => now(Move(Extreme(Up)))
    | "ArrowDown" => now(Move(Extreme(Down)))
    | _ => None
    }
  | {key: D(key), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    // | "k" => Some(ReparseCurrentEditor)
    | "/" => Some(Assistant(Prompt(TyDi)))
    | _ when is_digit(key) => Some(SwitchScratchSlide(int_of_string(key)))
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
