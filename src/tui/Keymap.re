open Haz3lcore;

/* Maps input events to app-level actions. Editor keys delegate to the
   same Web.Keyboard.handle_key_event the web app uses, so the two
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
  | PasteText(s) => Some(Perform(Paste(s)))
  | Mouse(m) => Some(Mouse(m))
  | Editor(k) =>
    switch (k) {
    /* Web maps Ctrl+S to PrettyPrint; the TUI reserves Ctrl+S for Save,
       so PrettyPrint moves to Alt+P. */
    | {key: D("p"), alt: Down, ctrl: Up, meta: Up, shift: Up, _} =>
      Some(Perform(PrettyPrint))
    | _ => Web.Keyboard.handle_key_event(k) |> Option.map(a => Perform(a))
    }
  };
