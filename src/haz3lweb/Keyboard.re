open Haz3lcore;

let is_digit = s => Re.Str.(string_match(regexp("^[0-9]$"), s, 0));
let is_f_key = s => Re.Str.(string_match(regexp("^F[0-9][0-9]*$"), s, 0));

let handle_key_event = (k: Key.t, ~model: Model.t): option(Update.t) => {
  let zipper = Editors.active_zipper(model.editors);
  let unselected = Zipper.unselect_and_zip(zipper);
  let (term, _) = MakeTerm.go(unselected);
  let (_, suggestions) = Statics.mk_map_and_inference_solutions(term);
  let global_inference_info =
    InferenceResult.mk_global_inference_info(
      model.settings.core.inference,
      suggestions,
    );
  let acceptSuggestionIfAvailable =
    InferenceView.acceptSuggestionIfAvailable(global_inference_info, zipper);
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
    | "F7" => Some(Benchmark(Start))
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
    | (Up, "Tab") => acceptSuggestionIfAvailable(Some(DoTheThing))
    | (Up, "F12") => now(Jump(BindingSiteOfIndicatedVar, Left))
    | (Down, "Tab") =>
      acceptSuggestionIfAvailable(Some(MoveToNextHole(Left)))
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
    | "Z"
    | "z" => Some(Redo)
    | "ArrowLeft" => now(Select(Resize(Extreme(Left(ByToken)))))
    | "ArrowRight" => now(Select(Resize(Extreme(Right(ByToken)))))
    | "ArrowUp" => now(Select(Resize(Extreme(Up))))
    | "ArrowDown" => now(Select(Resize(Extreme(Down))))
    | _ => None
    }
  | {key: D(key), sys: PC, shift: Down, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "Z"
    | "z" => Some(Redo)
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
    | "k" => Some(ReparseCurrentEditor)
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
    | "z" => Some(Undo)
    | "d" => now(Select(Term(Current)))
    | "p" => Some(PerformAction(Pick_up))
    | "a" => now(Select(All))
    | "k" => Some(ReparseCurrentEditor)
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
