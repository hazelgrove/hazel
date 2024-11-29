open Util;
open Js_of_ocaml;
open Haz3lweb;
open Bonsai.Let_syntax;

let scroll_to_caret = ref(true);
let edit_action_applied = ref(true);
let last_edit_action = ref(JsUtil.timestamp());

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

let apply = (model, action, ~schedule_action): Model.t => {
  restart_caret_animation();

  let get_settings = (model: Model.t): Settings.t => model.settings;

  let settings = get_settings(model);
  let editing_mode =
    settings.editing_prompt
    || settings.editing_title
    || settings.editing_test_val_rep
    || settings.editing_mut_test_rep
    || settings.editing_impl_grd_rep
    || settings.editing_module_name;

  switch (action, editing_mode) {
  | (UpdateAction.PerformAction(Insert(_)), true) => model
  | (UpdateAction.PerformAction(Destruct(_)), true) => model
  | (UpdateAction.PerformAction(Move(_)), true) => model
  | (action, _) =>
    if (UpdateAction.is_edit(action)) {
      last_edit_action := JsUtil.timestamp();
      edit_action_applied := true;
    };
    if (Update.should_scroll_to_caret(action)) {
      scroll_to_caret := true;
    };
    last_edit_action := JsUtil.timestamp();
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
    | Ok(model) =>
      let new_prov_tests =
        Editors.obtain_new_prov_tests(
          model.editors,
          model.results,
          ~settings=model.settings.core,
        );
      let updated_model =
        model.settings.instructor_mode
          ? {
            ...model,
            editors: Editors.update_prov_tests(model.editors, new_prov_tests),
          }
          : model;
      updated_model;
    | Error(FailedToPerform(err)) =>
      print_endline(Update.Failure.show(FailedToPerform(err)));
      model;
    | Error(err) =>
      print_endline(Update.Failure.show(err));
      model;
    };
  };
};

let app =
  Bonsai.state_machine0(
    (module Model),
    (module Update),
    ~apply_action=
      (~inject, ~schedule_event) =>
        apply(~schedule_action=x => schedule_event(inject(x))),
    ~default_model=Model.load(Model.blank),
  );

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
  let%sub app = app;
  let%sub () = {
    on_startup(
      Bonsai.Value.map(~f=((_model, inject)) => inject(Startup), app),
    );
  };
  let%sub after_display = {
    let%arr (_model, inject) = app;
    if (scroll_to_caret.contents) {
      scroll_to_caret := false;
      JsUtil.scroll_cursor_into_view_if_needed();
    };
    if (edit_action_applied^
        && JsUtil.timestamp()
        -. last_edit_action^ > 1000.0) {
      /* If an edit action has been applied, but no other edit action
         has been applied for 1 second, save the model. */
      edit_action_applied := false;
      print_endline("Saving...");
      inject(Update.Save);
    } else {
      Ui_effect.Ignore;
    };
  };
  let%sub () = Bonsai.Edge.after_display(after_display);
  let%arr (model, inject) = app;
  Haz3lweb.Page.view(~inject, model);
};

switch (JsUtil.Fragment.get_current()) {
| Some("debug") => DebugMode.go()
| _ => Bonsai_web.Start.start(view, ~bind_to_element_with_id="container")
};
