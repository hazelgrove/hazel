open Haz3lcore;

let is_printable = s => Re.Str.(string_match(regexp("^[ -~]$"), s, 0));
let is_digit = s => Re.Str.(string_match(regexp("^[0-9]$"), s, 0));
let is_f_key = s => Re.Str.(string_match(regexp("^F[0-9][0-9]*$"), s, 0));

let update_double_tap = (model: Model.t): list(Update.t) => {
  let cur_time = JsUtil.timestamp();
  switch (model.double_tap) {
  | None => [UpdateDoubleTap(Some(cur_time))]
  | Some(prev_time) =>
    if (cur_time -. prev_time < 400.) {
      [
        UpdateDoubleTap(None),
        // PerformAction(RotateBackpack) // TODO(cyrus) disabling double tab here, but would be good to give it a different hotkey
      ];
    } else {
      [UpdateDoubleTap(Some(cur_time))];
    }
  };
};

let handle_key_event = (k: Key.t, ~model: Model.t): list(Update.t) => {
  let zipper = Editors.get_zipper(model.editors);
  let unselected = Zipper.unselect_and_zip(zipper);
  let (term, _) = MakeTerm.go(unselected);
  let (_, global_inference_solutions) = Statics.mk_map_and_annotations(term);
  let global_inference_info =
    InferenceResult.mk_global_inference_info(
      model.langDocMessages.annotations,
      global_inference_solutions,
    );
  let restricted = Backpack.restricted(zipper.backpack);
  let now = a => [Update.PerformAction(a) /*Update.UpdateDoubleTap(None)*/];
  let now_save_u = u => Update.[u, Save] /*UpdateDoubleTap(None)*/;
  let now_save = a => now_save_u(PerformAction(a)); // TODO move saving logic out of keyboard handling code to avoid bugs if we start using other input modalities
  let print = str => str |> print_endline |> (_ => []);
  let toggle = m => (m := ! m^) |> (_ => []);
  switch (k) {
  | {key: U(key), _} =>
    switch (key) {
    | "Shift" => [] // NOTE: don't update double_tap here
    | "Alt" => [SetShowBackpackTargets(false)]
    | _ => [UpdateDoubleTap(None)]
    }
  | {key: D(key), sys: _, shift: Down, meta: Up, ctrl: Up, alt: Up}
      when is_f_key(key) =>
    switch (key) {
    | "F1" => print(Log.serialize())
    | "F2" => print(Zipper.show(zipper))
    | "F3" => toggle(Log.debug_update)
    | "F4" => toggle(Log.debug_keystroke)
    | "F5" => toggle(Log.debug_zipper)
    | "F6" => []
    | "F7" => []
    | "F8" => []
    | "F10" =>
      Log.reset_json_log();
      [];
    | _ => []
    }
  | {key: D(key), sys: _, shift, meta: Up, ctrl: Up, alt: Up} =>
    switch (shift, key) {
    | (Up, "ArrowLeft") => now(Move(Local(Left(ByChar))))
    | (Up, "ArrowRight") => now(Move(Local(Right(ByChar))))
    | (Up, "ArrowUp") => now(Move(Local(Up)))
    | (Up, "ArrowDown") => now(Move(Local(Down)))
    | (Up, "Home") => now(Move(Extreme(Left(ByToken))))
    | (Up, "End") => now(Move(Extreme(Right(ByToken))))
    | (Up, "Backspace") => now_save(Destruct(Left))
    | (Up, "Delete") => now_save(Destruct(Right))
    | (Up, "Escape") => now(Unselect)
    | (Up, "Tab") => now_save(Put_down) //TODO: if empty, move to next hole
    | (Up, "F12") => now(Jump(BindingSiteOfIndicatedVar))
    | (Down, "ArrowLeft") => now(Select(Resize(Local(Left(ByToken)))))
    | (Down, "ArrowRight") => now(Select(Resize(Local(Right(ByToken)))))
    | (Down, "ArrowUp") => now(Select(Resize(Local(Up))))
    | (Down, "ArrowDown") => now(Select(Resize(Local(Down))))
    | (_, "Shift") => update_double_tap(model)
    | (_, "Enter") =>
      //TODO(andrew): using funky char to avoid weird regexp issues with using \n
      let retrieve_string = (): option(string) => {
        open Util.OptUtil.Syntax;
        let* (p, _) = Zipper.representative_piece(zipper);
        InferenceResult.get_recommended_string(
          ~global_inference_info,
          Piece.id(p),
        );
      };
      switch (retrieve_string()) {
      | Some(typ_string) =>
        let explode = s =>
          List.init(String.length(s), i => String.make(1, s.[i]));
        typ_string
        |> explode
        |> List.filter(s => s != "?")
        |> List.map(str => now_save(Insert(str)))
        |> List.flatten;
      | None => now_save(Insert(Whitespace.linebreak))
      };
    | _ when Form.is_valid_char(key) && String.length(key) == 1 =>
      /* TODO(andrew): length==1 is hack to prevent things
         like F5 which are now valid tokens and also weird
         unicode shit which is multichar i guess */
      now_save(Insert(key))
    | _ => []
    }
  | {key: D(key), sys: Mac, shift: Down, meta: Down, ctrl: Up, alt: Up} =>
    switch (key) {
    | "Z"
    | "z" => now_save_u(Redo)
    | "ArrowLeft" => now(Select(Resize(Extreme(Left(ByToken)))))
    | "ArrowRight" => now(Select(Resize(Extreme(Right(ByToken)))))
    | "ArrowUp" => now(Select(Resize(Extreme(Up))))
    | "ArrowDown" => now(Select(Resize(Extreme(Down))))
    | _ => []
    }
  | {key: D(key), sys: PC, shift: Down, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "Z"
    | "z" => now_save_u(Redo)
    | "ArrowLeft" => now(Select(Resize(Local(Left(ByToken)))))
    | "ArrowRight" => now(Select(Resize(Local(Right(ByToken)))))
    | "ArrowUp" => now(Select(Resize(Local(Up))))
    | "ArrowDown" => now(Select(Resize(Local(Down))))
    | "Home" => now(Select(Resize(Extreme(Up))))
    | "End" => now(Select(Resize(Extreme(Down))))
    | _ => []
    }
  | {key: D(key), sys: Mac, shift: Up, meta: Down, ctrl: Up, alt: Up} =>
    switch (key) {
    | "z" => now_save_u(Undo)
    | "d" => now(Select(Term(Current)))
    | "p" => now(Pick_up)
    | "a" => now(Move(Extreme(Up))) @ now(Select(Resize(Extreme(Down))))
    | "k" => [ResetCurrentEditor]
    | _ when is_digit(key) => [SwitchSlide(int_of_string(key))]
    | "ArrowLeft" => now(Move(Extreme(Left(ByToken))))
    | "ArrowRight" => now(Move(Extreme(Right(ByToken))))
    | "ArrowUp" => now(Move(Extreme(Up)))
    | "ArrowDown" => now(Move(Extreme(Down)))
    | _ => []
    }
  | {key: D(key), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "z" => now_save_u(Undo)
    | "d" => now(Select(Term(Current)))
    | "p" => now(Pick_up)
    | "a" => now(Move(Extreme(Up))) @ now(Select(Resize(Extreme(Down))))
    | "k" => [ResetCurrentEditor]
    | _ when is_digit(key) => [SwitchSlide(int_of_string(key))]
    | "ArrowLeft" => now(Move(Local(Left(ByToken))))
    | "ArrowRight" => now(Move(Local(Right(ByToken))))
    | "Home" => now(Move(Extreme(Up)))
    | "End" => now(Move(Extreme(Down)))
    | _ => []
    }
  | {key: D(key), sys: Mac, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "a" => now(Move(Extreme(Left(ByToken))))
    | "e" => now(Move(Extreme(Right(ByToken))))
    | _ => []
    }
  | {key: D(key), sys, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    switch (sys, key) {
    | (_, "ArrowLeft") when restricted =>
      now(MoveToBackpackTarget(Left(ByToken)))
    | (_, "ArrowRight") when restricted =>
      now(MoveToBackpackTarget(Right(ByToken)))
    | (Mac, "ArrowLeft") => now(Move(Local(Left(ByToken))))
    | (Mac, "ArrowRight") => now(Move(Local(Right(ByToken))))
    | (_, "Alt") => [SetShowBackpackTargets(true), UpdateDoubleTap(None)]
    | (_, "ArrowUp") => now(MoveToBackpackTarget(Up))
    | (_, "ArrowDown") => now(MoveToBackpackTarget(Down))
    | _ => []
    }
  | _ => []
  };
};
