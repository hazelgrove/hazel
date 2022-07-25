open Core;

let is_printable = s => Re.Str.(string_match(regexp("^[ -~]$"), s, 0));
let is_digit = s => Re.Str.(string_match(regexp("^[0-9]$"), s, 0));
let is_f_key = s => Re.Str.(string_match(regexp("^F[0-9][0-9]*$"), s, 0));

let update_double_tap = (model: Model.t): list(Update.t) => {
  let cur_time = JsUtil.timestamp();
  switch (model.double_tap) {
  | None => [UpdateDoubleTap(Some(cur_time))]
  | Some(prev_time) =>
    if (cur_time -. prev_time < 400.) {
      [UpdateDoubleTap(None), PerformAction(RotateBackpack)];
    } else {
      [UpdateDoubleTap(Some(cur_time))];
    }
  };
};

let handle_key_event = (k: Key.t, ~model): list(Update.t) => {
  let zipper = Model.get_zipper(model);
  let restricted = Backpack.restricted(zipper.backpack);
  let now = a => [Update.PerformAction(a), Update.UpdateDoubleTap(None)];
  let now_save_u = u => Update.[u, Save, UpdateDoubleTap(None)];
  let now_save = a => now_save_u(PerformAction(a));
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
    | "F1" => print(Log.get_json_update_log_string())
    | "F2" => print(Zipper.show(zipper))
    | "F3" => toggle(Log.debug_update)
    | "F4" => toggle(Log.debug_keystoke)
    | "F5" => toggle(Log.debug_zipper)
    | "F6" => [Load]
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
    | (Down, "ArrowLeft") => now(Select(Local(Left(ByToken))))
    | (Down, "ArrowRight") => now(Select(Local(Right(ByToken))))
    | (Down, "ArrowUp") => now(Select(Local(Up)))
    | (Down, "ArrowDown") => now(Select(Local(Down)))
    | (_, "Shift") => update_double_tap(model)
    | (_, "Enter") =>
      //TODO(andrew): using funky char to avoid weird regexp issues with using \n
      now_save(Insert(Whitespace.linebreak))
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
    | "ArrowLeft" => now(Select(Extreme(Left(ByToken))))
    | "ArrowRight" => now(Select(Extreme(Right(ByToken))))
    | "ArrowUp" => now(Select(Extreme(Up)))
    | "ArrowDown" => now(Select(Extreme(Down)))
    | _ => []
    }
  | {key: D(key), sys: PC, shift: Down, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "Z"
    | "z" => now_save_u(Redo)
    | "ArrowLeft" => now(Select(Local(Left(ByToken))))
    | "ArrowRight" => now(Select(Local(Right(ByToken))))
    | "ArrowUp" => now(Select(Local(Up)))
    | "ArrowDown" => now(Select(Local(Down)))
    | "Home" => now(Select(Extreme(Up)))
    | "End" => now(Select(Extreme(Down)))
    | _ => []
    }
  | {key: D(key), sys: Mac, shift: Up, meta: Down, ctrl: Up, alt: Up} =>
    switch (key) {
    | "z" => now_save_u(Undo)
    | "x" => now(Pick_up)
    | "v" => now(Put_down)
    | "a" => now(Move(Extreme(Up))) @ now(Select(Extreme(Down)))
    | _ when is_digit(key) => [SwitchEditor(int_of_string(key))]
    | "ArrowLeft" => now(Move(Extreme(Left(ByToken))))
    | "ArrowRight" => now(Move(Extreme(Right(ByToken))))
    | "ArrowUp" => now(Move(Extreme(Up)))
    | "ArrowDown" => now(Move(Extreme(Down)))
    | _ => []
    }
  | {key: D(key), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
    switch (key) {
    | "z" => now_save_u(Undo)
    | "x" => now(Pick_up)
    | "v" => now(Put_down)
    | "a" => now(Move(Extreme(Up))) @ now(Select(Extreme(Down)))
    | _ when is_digit(key) => [SwitchEditor(int_of_string(key))]
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
