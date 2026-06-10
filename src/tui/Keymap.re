open Haz3lcore;

/* Maps input events to app-level actions. Editor keys delegate to the
   same Keyboard.handle_key_event the web app uses, so the two
   frontends share one keymap. TUI-only concerns (save/quit/undo/paging)
   are layered on top. */

[@deriving show({with_path: false})]
type t =
  | Perform(Action.t)
  | Tab /* context-dependent; resolved against the zipper in Update */
  | Save
  | Quit
  | Undo
  | Redo
  | PageUp
  | PageDown
  | ToggleResult
  | ToggleInspector
  /* screen coordinates; App translates to buffer coordinates */
  | Mouse(AnsiInput.mouse);

let handle = (ev: AnsiInput.event): option(t) =>
  switch (ev) {
  | Tui(Quit) => Some(Quit)
  | Tui(Save) => Some(Save)
  | Tui(Undo) => Some(Undo)
  | Tui(Redo) => Some(Redo)
  | Tui(TabKey) => Some(Tab)
  | Tui(ShiftTab) => Some(Perform(Move(Goal(NextProblem(Left)))))
  | Tui(PageUp) => Some(PageUp)
  | Tui(PageDown) => Some(PageDown)
  | Tui(ToggleResultPane) => Some(ToggleResult)
  | Tui(ToggleInspector) => Some(ToggleInspector)
  | PasteText(s) => Some(Perform(Paste(s)))
  | Mouse(m) => Some(Mouse(m))
  | Editor(k) =>
    switch (k) {
    /* Web maps Ctrl+S to PrettyPrint; the TUI reserves Ctrl+S for Save,
       so PrettyPrint moves to Alt+P. */
    | {key: D("p"), alt: Down, ctrl: Up, meta: Up, shift: Up, _} =>
      Some(Perform(PrettyPrint))
    /* macOS terminals without "Option as Meta" don't send ESC+letter
       for Option chords; they type the composed character instead
       (Option+F = ƒ, Option+T = †, Option+P = π). None of these are
       valid Hazel tokens, so receiving one can only mean the chord —
       same trick the web keymap uses for its Mac bindings. */
    | {key: D("ƒ"), _} /* ƒ */ =>
      Some(Perform(Project(SetIndicated(Specific(Fold)))))
    | {key: D("†"), _} /* † */ => Some(Perform(Probe(ToggleStatics)))
    | {key: D("π"), _} /* π */ => Some(Perform(PrettyPrint))
    | _ => Keyboard.handle_key_event(k) |> Option.map(a => Perform(a))
    }
  };
