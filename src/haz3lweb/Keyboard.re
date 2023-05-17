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
    //TODO(andrew): selectively (~erase_buffer=true)
    let get_term = z => z |> Zipper.unselect_and_zip |> MakeTerm.go |> fst;
    switch (key) {
    | "F1" => zipper |> Zipper.show |> print
    | "F2" => zipper |> Zipper.unselect_and_zip |> Segment.show |> print
    | "F3" =>
      zipper
      |> Zipper.seg_without_buffer
      |> MakeTerm.go
      |> fst
      |> TermBase.UExp.show
      |> print
    | "F4" => zipper |> get_term |> Statics.mk_map |> Statics.Map.show |> print
    | "F5" =>
      let term = zipper |> get_term;
      let map = term |> Statics.mk_map;
      Interface.eval_to_result(map, term) |> ProgramResult.show |> print;
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
    | "F7" => [Update.Script(StartTest())]
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
    | (Up, "Escape") => now(Unselect(None))
    | (Up, "Tab") =>
      Selection.is_buffer(zipper.selection)
        ? [Agent(AcceptSuggestion), Save]
        : Zipper.can_put_down(zipper)
            ? [PerformAction(Put_down), Save]
            : [MoveToNextHole(Right), Save]
    | (Up, "F12") => now(Jump(BindingSiteOfIndicatedVar))
    | (Down, "ArrowLeft") => now(Select(Resize(Local(Left(ByToken)))))
    | (Down, "ArrowRight") => now(Select(Resize(Local(Right(ByToken)))))
    | (Down, "ArrowUp") => now(Select(Resize(Local(Up))))
    | (Down, "ArrowDown") => now(Select(Resize(Local(Down))))
    | (Down, "Home") => now(Select(Resize(Extreme(Left(ByToken)))))
    | (Down, "End") => now(Select(Resize(Extreme(Right(ByToken)))))
    | (_, "Shift") => update_double_tap(model)
    | (_, "Enter") => now_save(Insert(Form.linebreak))
    | _ when /*Form.is_valid_char(key) &&*/ String.length(key) == 1 =>
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
    | "p" =>
      print_endline("DEBUG: CMD+P");
      let f = (z, id_gen) =>
        Util.OptUtil.Syntax.(
          {
            let s = (z, id_gen);
            let* s = Insert.go(Form.linebreak, s);
            let* s = Insert.go(Form.linebreak, s);
            let* s = Insert.go(Form.linebreak, s);
            Insert.go(Form.linebreak, s);
          }
        );
      [
        Update.PerformAction(
          RemoteAction(Action.Extreme(Right(ByToken)), f),
        ),
      ];
    | "b" =>
      print_endline("DEBUG: CMD+B");
      let f = (z, id_gen) =>
        Util.OptUtil.Syntax.(
          {
            let s = (z, id_gen);
            let* s = Destruct.go(Right, s);
            let* s = Destruct.go(Right, s);
            let* s = Destruct.go(Right, s);
            Destruct.go(Right, s);
          }
        );
      [
        Update.PerformAction(
          RemoteAction(Action.Extreme(Right(ByToken)), f),
        ),
      ];
    | "z" => now_save_u(Undo)
    | "d" => now(Select(Term(Current)))
    //| "p" => now(Pick_up)
    | "a" => now(Move(Extreme(Up))) @ now(Select(Resize(Extreme(Down))))
    | "k" => [ResetCurrentEditor]
    | "e" => [Execute("")]
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
    | "e" => [Execute("")]
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
