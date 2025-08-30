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
    | (Up, "ArrowLeft") => now(Move(Local(Left, ByChar)))
    | (Up, "ArrowRight") => now(Move(Local(Right, ByChar)))
    | (Up, "ArrowUp") => now(Move(Vertical(Up)))
    | (Up, "ArrowDown") => now(Move(Vertical(Down)))
    | (Up, "Home") => now(Move(Start))
    | (Up, "End") => now(Move(End))
    | (Up, "Backspace") => now(Destruct(Left))
    | (Up, "Delete") => now(Destruct(Right))
    | (Up, "Escape") => now(Unselect(None))
    | (Up, "F12") => now(Move(Goal(BindingSiteOfIndicatedVar)))
    | (Down, "Tab") => now(Move(Goal(Hole(Left))))
    | (Down, "ArrowLeft") => now(Select(Resize(Local(Left, ByToken))))
    | (Down, "ArrowRight") => now(Select(Resize(Local(Right, ByToken))))
    | (Down, "ArrowUp") => now(Select(Resize(Vertical(Up))))
    | (Down, "ArrowDown") => now(Select(Resize(Vertical(Down))))
    | (Down, "Home") => now(Select(Resize(Start)))
    | (Down, "End") => now(Select(Resize(End)))
    | (_, "Enter") => now(Insert(Token.linebreak))
    | _ when String.length(key) == 1 =>
      /* Note: length==1 prevent specials like
       * SHIFT from being captured here */
      now(Insert(key))
    | _ => None
    }
  | {key: D(key), sys: Mac, shift: Down, meta: Down, ctrl: Up, alt: Up} =>
    switch (key) {
    | "ArrowLeft" => now(Select(Resize(Line(Left))))
    | "ArrowRight" => now(Select(Resize(Line(Right))))
    | "ArrowUp" => now(Select(Resize(Start)))
    | "ArrowDown" => now(Select(Resize(End)))
    | _ => None
    }
  | {key: D(key), sys: PC, shift: Down, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "ArrowLeft" => now(Select(Resize(Local(Left, ByToken))))
    | "ArrowRight" => now(Select(Resize(Local(Right, ByToken))))
    | "ArrowUp" => now(Select(Resize(Vertical(Up))))
    | "ArrowDown" => now(Select(Resize(Vertical(Down))))
    | "Home" => now(Select(Resize(Start)))
    | "End" => now(Select(Resize(End)))
    | _ => None
    }
  | {key: D(key), sys: Mac, shift: Up, meta: Down, ctrl: Up, alt: Up} =>
    switch (key) {
    | "d" => now(Select(Term(Current)))
    | "a" => now(Select(All))
    | "/" => Some(Buffer(Set(TyDi)))
    | "ArrowLeft" => now(Move(Line(Left)))
    | "ArrowRight" => now(Move(Line(Right)))
    | "ArrowUp" => now(Move(Start))
    | "ArrowDown" => now(Move(End))
    | _ => None
    }
  | {key: D(key), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "d" => now(Select(Term(Current)))
    | "a" => now(Select(All))
    | "/" => Some(Buffer(Set(TyDi)))
    | "ArrowLeft" => now(Move(Local(Left, ByToken)))
    | "ArrowRight" => now(Move(Local(Right, ByToken)))
    | "Home" => now(Move(Start))
    | "End" => now(Move(End))
    | _ => None
    }
  | {key: D(key), sys: Mac, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "a" => now(Move(Line(Left)))
    | "e" => now(Move(Line(Right)))
    | "c" => now(AssistantComposition(Nav(GoToChild("", Some(0)))))
    | "," => now(AssistantComposition(Nav(GoToSibling(Stepwise(Left)))))
    | "." => now(AssistantComposition(Nav(GoToSibling(Stepwise(Right)))))
    | "p" => now(AssistantComposition(Nav(GoToParent)))
    // vvv Debug shortcuts vvv
    | "q" => now(AssistantComposition(Edit(UpdatePattern("x : ? -> ?"))))
    | "w" => now(AssistantComposition(Edit(UpdateDefinition("100"))))
    | "r" => now(AssistantComposition(Edit(UpdateBody("x * 2"))))
    | "t" => now(AssistantComposition(Edit(DeleteBody)))
    | "y" => now(AssistantComposition(Edit(DeleteBindingClause)))
    | "u" => now(AssistantComposition(Edit(InsertBefore("let s = ? in"))))
    | "i" => now(AssistantComposition(Edit(InsertAfter("let r = ? in"))))
    | "g" => now(AssistantComposition(Nav(GoToBindingSite("f", None))))
    | "o" =>
      now(
        AssistantComposition(
          Edit(UpdateBindingClause("let hmph = 900 in")),
        ),
      )
    | _ => None
    }
  | {key: D("f"), sys: PC, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    Some(Project(SetIndicated(Specific(Fold))))
  | {key: D("ƒ"), sys: Mac, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    /* Curly ƒ is what holding option turns f into on Mac */
    Some(Project(SetIndicated(Specific(Fold))))
  | {key: D("v"), sys: PC, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    Some(Project(SetIndicated(Specific(Probe))))
  | {key: D("√"), sys: Mac, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    /* √ is what holding option turns f into on Mac */
    Some(Project(SetIndicated(Specific(Probe))))
  | {key: D("t"), sys: PC, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    Some(Project(SetIndicated(Specific(Info))))
  | {key: D("†"), sys: Mac, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    /* † is what holding option turns t into on Mac */
    Some(Project(SetIndicated(Specific(Info))))
  | {key: D("l"), sys: PC, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    Some(Project(SetIndicated(ChooseLivelit)))
  | {key: D("¬"), sys: Mac, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    /* † is what holding option turns t into on Mac */
    Some(Project(SetIndicated(ChooseLivelit)))
  | {key: D("µ"), sys: Mac, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    print_endline("˜dump");
    Some(Dump);
  | {key: D(key), sys: _, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    switch (key) {
    | "ArrowLeft" => now(Move(Local(Left, ByToken)))
    | "ArrowRight" => now(Move(Local(Right, ByToken)))
    | _ => None
    }
  | _ => None
  };
};
