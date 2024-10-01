open Haz3lcore;
open Util;

let is_digit = s => StringUtil.(match(regexp("^[0-9]$"), s));
let is_f_key = s => StringUtil.(match(regexp("^F[0-9][0-9]*$"), s));

let meta = (sys: Key.sys): string => {
  switch (sys) {
  | Mac => "cmd"
  | PC => "ctrl"
  };
};

let handle_key_event = (k: Key.t): option(Action.t) => {
  let now = (a: Action.t) => Some(a);
  switch (k) {
  | {key: U(key), _} =>
    /* Keu-UPpEvents:
       NOTE: Remember that since there is a keyup for every
       keydown, making an update here may trigger an entire
       extra redraw, contingent on model.cutoff */
    switch (key) {
    | _ => None
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
    | "d" => now(Select(Term(Current)))
    | "p" => now(Pick_up)
    | "a" => now(Select(All))
    | "k" => Some(Reparse)
    | "/" => Some(Buffer(Set(TyDi)))
    | "ArrowLeft" => now(Move(Extreme(Left(ByToken))))
    | "ArrowRight" => now(Move(Extreme(Right(ByToken))))
    | "ArrowUp" => now(Move(Extreme(Up)))
    | "ArrowDown" => now(Move(Extreme(Down)))
    | _ => None
    }
  | {key: D(key), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "d" => now(Select(Term(Current)))
    | "p" => now(Pick_up)
    | "a" => now(Select(All))
    | "k" => Some(Reparse)
    | "/" => Some(Buffer(Set(TyDi)))
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
    Some(Project(ToggleIndicated(Fold)))
  | {key: D("ƒ"), sys: Mac, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    /* Curly ƒ is what holding option turns f into on Mac */
    Some(Project(ToggleIndicated(Fold)))
  | {key: D(key), sys: _, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    switch (key) {
    | "ArrowLeft" => now(MoveToBackpackTarget(Left(ByToken)))
    | "ArrowRight" => now(MoveToBackpackTarget(Right(ByToken)))
    | "ArrowUp" => now(MoveToBackpackTarget(Up))
    | "ArrowDown" => now(MoveToBackpackTarget(Down))
    | _ => None
    }
  | _ => None
  };
};
