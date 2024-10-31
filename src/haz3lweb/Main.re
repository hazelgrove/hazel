open Util;
open Js_of_ocaml;
open Haz3lweb;
open Bonsai.Let_syntax;

let scroll_to_caret = ref(true);

let restart_caret_animation = () =>
  // necessary to trigger reflow
  // <https://css-tricks.com/restart-css-animation/>
  try({
    let caret_elem = JsUtil.get_elem_by_id("caret");
    caret_elem##.classList##remove(Js.string("blink"));
    let _ = caret_elem##getBoundingClientRect;
    caret_elem##.classList##add(Js.string("blink"));
  }) {
  | _ => ()
  };

let apply = (model, action, ~schedule_action, ~schedule_autosave): Model.t => {
  restart_caret_animation();
  if (UpdateAction.is_edit(action)) {
    schedule_autosave(
      BonsaiUtil.Alarm.Action.SetAlarm(
        Core.Time_ns.add(Core.Time_ns.now(), Core.Time_ns.Span.of_sec(2.0)),
      ),
    );
  } else {
    schedule_autosave(
      BonsaiUtil.Alarm.Action.SnoozeAlarm(
        Core.Time_ns.add(Core.Time_ns.now(), Core.Time_ns.Span.of_sec(2.0)),
      ),
    );
  };
  if (Update.should_scroll_to_caret(action)) {
    scroll_to_caret := true;
  };
  switch (
    try({
      let new_model = Update.apply(model, action, ~schedule_action);
      Log.update(action);
      new_model;
    }) {
    | exc =>
      Printf.printf(
        "ERROR: Exception during apply: %s\n",
        Printexc.to_string(exc),
      );
      Error(Exception(Printexc.to_string(exc)));
    }
  ) {
  | Ok(model) => model
  | Error(FailedToPerform(err)) =>
    print_endline(Update.Failure.show(FailedToPerform(err)));
    model;
  | Error(err) =>
    print_endline(Update.Failure.show(err));
    model;
  };
};

/* This subcomponent is used to run an effect once when the app starts up,
   After the first draw */
let on_startup = effect => {
  let%sub startup_completed = Bonsai.toggle'(~default_model=false);
  let%sub after_display = {
    switch%sub (startup_completed) {
    | {state: false, set_state, _} =>
      let%arr effect = effect
      and set_state = set_state;
      Bonsai.Effect.Many([set_state(true), effect]);
    | {state: true, _} => Bonsai.Computation.return(Ui_effect.Ignore)
    };
  };
  Bonsai.Edge.after_display(after_display);
};

let view = {
  let%sub save_scheduler = BonsaiUtil.Alarm.alarm;
  let%sub app =
    Bonsai.state_machine1(
      (module Model),
      (module Update),
      ~apply_action=
        (~inject, ~schedule_event, input) => {
          let schedule_action = x => schedule_event(inject(x));
          let schedule_autosave = action =>
            switch (input) {
            | Active((_, alarm_inject)) =>
              schedule_event(alarm_inject(action))
            | Inactive => ()
            };
          apply(~schedule_action, ~schedule_autosave);
        },
      ~default_model=Model.load(Model.blank),
      save_scheduler,
    );
  let%sub () = {
    on_startup(
      Bonsai.Value.map(~f=((_model, inject)) => inject(Startup), app),
    );
  };
  let after_display = {
    Bonsai.Effect.of_sync_fun(
      () =>
        if (scroll_to_caret.contents) {
          scroll_to_caret := false;
          JsUtil.scroll_cursor_into_view_if_needed();
        },
      (),
    );
  };
  let save_effect = Bonsai.Value.map(~f=((_, g)) => g(Update.Save), app);
  let%sub () = BonsaiUtil.Alarm.listen(save_scheduler, ~event=save_effect);
  let%sub () =
    Bonsai.Edge.after_display(after_display |> Bonsai.Value.return);
  let%arr (model, inject) = app;
  Haz3lweb.Page.view(~inject, model);
};

switch (JsUtil.Fragment.get_current()) {
| Some("debug") => DebugMode.go()
| _ => Bonsai_web.Start.start(view, ~bind_to_element_with_id="container")
};
