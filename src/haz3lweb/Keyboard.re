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
  let restricted = Backpack.restricted(zipper.backpack);
  let now = a => [Update.PerformAction(a) /*Update.UpdateDoubleTap(None)*/];
  let now_save_u = u => Update.[u, Save] /*UpdateDoubleTap(None)*/;
  let now_save = a => now_save_u(PerformAction(a)); // TODO move saving logic out of keyboard handling code to avoid bugs if we start using other input modalities
  let print = str => str |> print_endline |> (_ => []);
  switch (k) {
  | {key: U(key), _} =>
    switch (key) {
    | "Shift" => [] // NOTE: don't update double_tap here
    | "Alt" => [SetShowBackpackTargets(false)]
    | _ => [UpdateDoubleTap(None)]
    }
  | {key: D(key), sys: _, shift: Down, meta: Up, ctrl: Up, alt: Up}
      when is_f_key(key) =>
    let index = Indicated.index(zipper);
    let get_term = z => z |> Zipper.unselect_and_zip |> MakeTerm.go |> fst;
    let get_elab = z => {
      let term = z |> get_term;
      let map = term |> Statics.mk_map;
      let index = index |> Util.OptUtil.and_then(Probe.get_exp_parent(map));
      Interface.elaborate(~probe_ids=Option.to_list(index), map, term);
    };
    switch (key) {
    | "F1" =>
      /* Print cursor info for current position */
      let map = zipper |> get_term |> Statics.mk_map;
      switch (index) {
      | Some(index) =>
        switch (Haz3lcore.Id.Map.find_opt(index, map)) {
        | Some(ci) => print(Info.show(ci))
        | _ => print("DEBUG: No CI found for index")
        }
      | _ => print("DEBUG: No indicated index")
      };
    | "F2" =>
      /* Print program zipper */
      zipper |> Zipper.show |> print
    | "F3" =>
      /* Print program as unzipped segment */
      zipper |> Zipper.unselect_and_zip |> Segment.show |> print
    | "F4" =>
      /* Print program term */
      zipper |> get_term |> TermBase.UExp.show |> print
    | "F5" =>
      /* Print entire static info map */
      zipper |> get_term |> Statics.mk_map |> Statics.show_map |> print
    | "F6" =>
      /* Print program elaboration */
      zipper |> get_elab |> DHExp.show |> print
    | "F7" =>
      /* Print program evaluation */
      zipper |> get_elab |> Interface.evaluate |> ProgramResult.show |> print
    | "F8" =>
      /* TODO(andrew): cleanup. testing for probe */
      /*zipper
        |> get_elab
        |> Interface.evaluate
        |> ProgramResult.get_state
        |> EvaluatorState.get_probes
        |> ProbeMap.process
        |> ProbeMap.show_processed_map
        |> print*/
      []
    | _ => []
    };
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
      now_save(Insert(Secondary.linebreak))
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
