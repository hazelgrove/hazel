open Haz3lcore;
open Util;

let is_printable = s => Re.Str.(string_match(regexp("^[ -~]$"), s, 0));
let is_digit = s => Re.Str.(string_match(regexp("^[0-9]$"), s, 0));
let is_f_key = s => Re.Str.(string_match(regexp("^F[0-9][0-9]*$"), s, 0));

let handle_key_event = (k: Key.t, ~model: Model.t): option(Update.t) => {
  let zipper = Editors.get_zipper(model.editors);
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
  let print = str => str |> print_endline |> (_ => None);
  switch (k) {
  | {key: U(key), _} =>
    /* NOTE: Remember that since there is a keyup for every
       keydown, making an update here may trigger an entire
       redraw, contingent on model.cutoff */
    switch (key) {
    | "Alt" => Some(SetShowBackpackTargets(false))
    | _ => None
    }
  | {key: D(key), sys: _, shift: Down, meta: Up, ctrl: Up, alt: Up}
      when is_f_key(key) =>
    let get_term = z => z |> Zipper.unselect_and_zip |> MakeTerm.go |> fst;
    switch (key) {
    | "F1" => zipper |> Zipper.show |> print
    | "F2" => zipper |> Zipper.unselect_and_zip |> Segment.show |> print
    | "F3" => zipper |> get_term |> TermBase.UExp.show |> print
    | "F4" => zipper |> get_term |> Statics.mk_map |> Statics.Map.show |> print
    | "F5" =>
      let term = zipper |> get_term;
      let map = term |> Statics.mk_map;
      Interface.get_result(map, term) |> ProgramResult.show |> print;
    | "F6" =>
      let index = Indicated.index(zipper);
      let map = zipper |> get_term |> Statics.mk_map;
      switch (index) {
      | Some(index) =>
        switch (Haz3lcore.Id.Map.find_opt(index, map)) {
        | Some(ci) => print(Info.show(ci))
        | _ => print("DEBUG: No CI found for index")
        }
      | _ => print("DEBUG: No indicated index")
      };
    | "F7" => Some(Benchmark(Start))
    | _ => None
    };
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
    | (Up, "Escape") => now(Unselect)
    | (Up, "Tab") =>
      Zipper.can_put_down(zipper)
        ? Some(PerformAction(Put_down)) : Some(MoveToNextHole(Right))
    | (Up, "F12") => now(Jump(BindingSiteOfIndicatedVar))
    | (Down, "Tab") => Some(MoveToNextHole(Left))
    | (Down, "ArrowLeft") => now(Select(Resize(Local(Left(ByToken)))))
    | (Down, "ArrowRight") => now(Select(Resize(Local(Right(ByToken)))))
    | (Down, "ArrowUp") => now(Select(Resize(Local(Up))))
    | (Down, "ArrowDown") => now(Select(Resize(Local(Down))))
    | (Down, "Home") => now(Select(Resize(Extreme(Left(ByToken)))))
    | (Down, "End") => now(Select(Resize(Extreme(Right(ByToken)))))
<<<<<<< HEAD
    | (_, "Shift") => update_double_tap(model)
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
        [UpdateAction.Paste(no_hole_marks)];
      | _ => now_save(Insert(Form.linebreak))
      };
=======
    | (_, "Enter") => now(Insert(Form.linebreak))
>>>>>>> dev
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
    | (_, "ArrowLeft") when restricted =>
      now(MoveToBackpackTarget(Left(ByToken)))
    | (_, "ArrowRight") when restricted =>
      now(MoveToBackpackTarget(Right(ByToken)))
    | (Mac, "ArrowLeft") => now(Move(Local(Left(ByToken))))
    | (Mac, "ArrowRight") => now(Move(Local(Right(ByToken))))
    | (_, "Alt") => Some(SetShowBackpackTargets(true))
    | (_, "ArrowUp") => now(MoveToBackpackTarget(Up))
    | (_, "ArrowDown") => now(MoveToBackpackTarget(Down))
    | _ => None
    }
  | _ => None
  };
};
