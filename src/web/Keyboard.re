open Haz3lcore;
open Util_web;

let is_f_key = s => StringUtil.(match(regexp("^F[0-9][0-9]*$"), s));

let meta = (): string => {
  let sys = Util_web.Os.is_mac^ ? Util_web.Key.Mac : PC;
  switch (sys) {
  | Mac => "cmd"
  | PC => "ctrl"
  };
};

/* Mouse-drag chunkiness. The "Character-level mouse" setting
 * (selection_chunkiness) sets the no-modifier default — applied in
 * Perform: off → smart rounding, on → pure char. Holding the modifier
 * (Alt on Mac / Ctrl on PC) while dragging selects the opposite. */
let mouse_modifier_chunk =
    (settings: Language.CoreSettings.t): Action.chunkiness =>
  settings.selection_chunkiness ? BySmart : ByChar;

/* Keyboard chunkiness. The keyboard favors fine control, so bare
 * Shift+Arrow is always char-level; holding the modifier (Alt/Ctrl)
 * switches to smart rounding. Independent of the mouse setting. */
let kbd_default_chunk: Action.chunkiness = ByChar;
let kbd_modifier_chunk: Action.chunkiness = BySmart;

let handle_key_event = (k: Key.t): option(Action.t) => {
  let now = (a: Action.t) => Some(a);
  let def = kbd_default_chunk;
  let modif = kbd_modifier_chunk;
  switch (k) {
  | {key: U(key), _} =>
    /* Keu-UPpEvents:
       NOTE: Remember that since there is a keyup for every
       keydown, making an update here may trigger an entire
       extra redraw, contingent on model.cutoff */
    switch (key) {
    | _ => None
    }
  | {key: D(key), sys: _, shift, meta: Up, ctrl: Up, alt: Up, _} =>
    switch (shift, key) {
    | (Up, "ArrowLeft") => now(Move(Local(Left, ByChar)))
    | (Up, "ArrowRight") => now(Move(Local(Right, ByChar)))
    | (Up, "ArrowUp") => now(Move(Vertical(Up, ByChar)))
    | (Up, "ArrowDown") => now(Move(Vertical(Down, ByChar)))
    | (Up, "Home") => now(Move(Line(Left)))
    | (Up, "End") => now(Move(Line(Right)))
    | (_, "Backspace") => now(Destruct(Left))
    | (_, "Delete") => now(Destruct(Right))
    | (Up, "Escape") => now(Unselect(None))
    | (Up, "F12") => now(Move(Goal(BindingSiteOfIndicatedVar)))
    | (Down, "Tab") => now(Move(Goal(NextProblem(Left))))
    | (Down, "ArrowLeft") => now(Select(Resize(Local(Left, def))))
    | (Down, "ArrowRight") => now(Select(Resize(Local(Right, def))))
    | (Down, "ArrowUp") => now(Select(Resize(Vertical(Up, def))))
    | (Down, "ArrowDown") => now(Select(Resize(Vertical(Down, def))))
    | (Down, "Home") => now(Select(Resize(Line(Left))))
    | (Down, "End") => now(Select(Resize(Line(Right))))
    | (_, "Enter") => now(Insert(Token.linebreak))
    | _ when Unicode.length(key) == 1 =>
      /* One grapheme cluster, which is exactly what KeyboardEvent.key
       * reports for a character key -- named keys ("Shift", "Enter",
       * "ArrowLeft", "F1", "Dead", "Process", ...) are all several
       * clusters long, so they still fall through. Counting BYTES here
       * (as this used to) silently dropped every non-ASCII key: `é` is
       * two bytes and `😀` is four. */
      now(Insert(key))
    | _ => None
    }
  | {key: D(key), sys: Mac, shift: Down, meta: Up, ctrl: Up, alt: Down, _} =>
    switch (key) {
    | "ArrowLeft" => now(Select(Resize(Local(Left, modif))))
    | "ArrowRight" => now(Select(Resize(Local(Right, modif))))
    | "ArrowUp" => now(Select(Resize(Vertical(Up, modif))))
    | "ArrowDown" => now(Select(Resize(Vertical(Down, modif))))
    | _ => None
    }
  | {key: D(key), sys: Mac, shift: Down, meta: Down, ctrl: Up, alt: Up, _} =>
    switch (key) {
    | "ArrowLeft" => now(Select(Resize(Line(Left))))
    | "ArrowRight" => now(Select(Resize(Line(Right))))
    | "ArrowUp" => now(Select(Resize(Start)))
    | "ArrowDown" => now(Select(Resize(End)))
    | _ => None
    }
  | {key: D(key), sys: PC, shift: Down, meta: Up, ctrl: Down, alt: Up, _} =>
    switch (key) {
    | "ArrowLeft" => now(Select(Resize(Local(Left, modif))))
    | "ArrowRight" => now(Select(Resize(Local(Right, modif))))
    | "ArrowUp" => now(Select(Resize(Vertical(Up, modif))))
    | "ArrowDown" => now(Select(Resize(Vertical(Down, modif))))
    | "Home" => now(Select(Resize(Start)))
    | "End" => now(Select(Resize(End)))
    | _ => None
    }
  | {key: D(key), sys: Mac, shift: Up, meta: Down, ctrl: Up, alt: Up, _} =>
    switch (key) {
    | "s" => now(PrettyPrint)
    | "d" => now(Select(Term(Current)))
    | "a" => now(Select(All))
    | "e" => now(Probe(ToggleManual))
    | "/" => Some(Buffer(Set(TyDi)))
    | "ArrowLeft" => now(Move(Line(Left)))
    | "ArrowRight" => now(Move(Line(Right)))
    | "ArrowUp" => now(Move(Start))
    | "ArrowDown" => now(Move(End))
    | _ => None
    }

  | {key: D(key), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up, _} =>
    switch (key) {
    | "s" => now(PrettyPrint)
    | "d" => now(Select(Term(Current)))
    | "a" => now(Select(All))
    | "e" => now(Probe(ToggleManual))
    | "/" => Some(Buffer(Set(TyDi)))
    | "ArrowLeft" => now(Move(Local(Left, ByToken)))
    | "ArrowRight" => now(Move(Local(Right, ByToken)))
    | "Home" => now(Move(Start))
    | "End" => now(Move(End))
    | _ => None
    }
  | {key: D(key), sys: Mac, shift: Up, meta: Up, ctrl: Down, alt: Up, _} =>
    switch (key) {
    | "a" => now(Move(Line(Left)))
    | "e" => now(Move(Line(Right)))
    | _ => None
    }
  | {key: D("f"), sys: PC, shift: Up, meta: Up, ctrl: Up, alt: Down, _} =>
    Some(Project(SetIndicated(Specific(Fold))))
  | {key: D("ƒ"), sys: Mac, shift: Up, meta: Up, ctrl: Up, alt: Down, _} =>
    /* Curly ƒ is what holding option turns f into on Mac */
    Some(Project(SetIndicated(Specific(Fold))))
  | {key: D("t"), sys: PC, shift: Up, meta: Up, ctrl: Up, alt: Down, _} =>
    Some(Probe(ToggleStatics))
  | {key: D("†"), sys: Mac, shift: Up, meta: Up, ctrl: Up, alt: Down, _} =>
    /* † is what holding option turns t into on Mac */
    Some(Probe(ToggleStatics))
  | {key: D("l"), sys: PC, shift: Up, meta: Up, ctrl: Up, alt: Down, _} =>
    Some(Project(SetIndicated(ChooseLivelit)))
  | {key: D("¬"), sys: Mac, shift: Up, meta: Up, ctrl: Up, alt: Down, _} =>
    /* † is what holding option turns t into on Mac */
    Some(Project(SetIndicated(ChooseLivelit)))
  | {key: D("µ"), sys: Mac, shift: Up, meta: Up, ctrl: Up, alt: Down, _} =>
    Some(Dump)
  | {key: D(key), sys: _, shift: Up, meta: Up, ctrl: Up, alt: Down, _} =>
    switch (key) {
    | "ArrowLeft" => now(Move(Local(Left, ByToken)))
    | "ArrowRight" => now(Move(Local(Right, ByToken)))
    | _ => None
    }
  | {key: D(key), sys: _, shift: Down, meta: Up, ctrl: Up, alt: Down, _} =>
    switch (key) {
    | "ArrowLeft" => now(Select(Resize(Local(Left, modif))))
    | "ArrowRight" => now(Select(Resize(Local(Right, modif))))
    | _ => None
    }
  | _ => None
  };
};
