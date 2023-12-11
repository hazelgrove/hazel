open Haz3lcore;
open Util;

let is_digit = s => Re.Str.(string_match(regexp("^[0-9]$"), s, 0));
let is_f_key = s => Re.Str.(string_match(regexp("^F[0-9][0-9]*$"), s, 0));

let handle_key_event = (k: Key.t, ~model: Model.t): option(Update.t) => {
  let zipper = Editors.active_zipper(model.editors);
  let unselected = Zipper.unselect_and_zip(zipper);
  let (term, _) = MakeTerm.go(unselected);
  let (_, ctx) = Statics.mk_map_and_inference_solutions(term);
  let global_inference_info =
    InferenceResult.mk_global_inference_info(
      model.langDocMessages.annotations,
      ctx,
    );
  let restricted = Backpack.restricted(zipper.backpack);
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
    | (Up, "Tab") => Some(DoTheThing)
    | (Up, "F12") => now(Jump(BindingSiteOfIndicatedVar))
    | (Down, "Tab") => Some(MoveToNextHole(Left))
    | (Down, "ArrowLeft") => now(Select(Resize(Local(Left(ByToken)))))
    | (Down, "ArrowRight") => now(Select(Resize(Local(Right(ByToken)))))
    | (Down, "ArrowUp") => now(Select(Resize(Local(Up))))
    | (Down, "ArrowDown") => now(Select(Resize(Local(Down))))
    | (Down, "Home") => now(Select(Resize(Extreme(Left(ByToken)))))
    | (Down, "End") => now(Select(Resize(Extreme(Right(ByToken)))))
    | (_, "Enter") =>
      let suggestion_opt = {
        open Util.OptUtil.Syntax;
        let+ (p, _) = Zipper.representative_piece(zipper);
        InferenceResult.get_suggestion_text_for_id(
          Piece.id(p),
          global_inference_info,
        );
      };
      switch (suggestion_opt) {
      | Some(Solvable(typ_filling))
      | Some(NestedInconsistency(typ_filling)) =>
        // question marks (holes) can't be inserted manually, so filter them out
        let join = List.fold_left((s, acc) => s ++ acc, "");
        let no_hole_marks =
          typ_filling
          |> StringUtil.to_list
          |> List.filter(s => s != "?" && s != "!")
          |> join;
        Some(UpdateAction.Paste(no_hole_marks));
      | _ => now(Insert(Form.linebreak))
      };
    | _ when Form.is_valid_char(key) && String.length(key) == 1 =>
      /* TODO(andrew): length==1 is hack to prevent things
         like F5 which are now valid tokens and also weird
         unicode shit which is multichar i guess */
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
