open Util;
open Js_of_ocaml;
open Incr_dom;
open Haz3lweb;
open Haz3lcore;

let scroll_to_caret = ref(true);
let edit_action_applied = ref(true);
let last_edit_action = ref(JsUtil.timestamp());

let observe_font_specimen = (id, update) =>
  ResizeObserver.observe(
    ~node=JsUtil.get_elem_by_id(id),
    ~f=
      (entries, _) => {
        let specimen = Js.to_array(entries)[0];
        let rect = specimen##.contentRect;
        update(
          Haz3lweb.FontMetrics.{
            row_height: rect##.bottom -. rect##.top,
            col_width: rect##.right -. rect##.left,
          },
        );
      },
    (),
  );

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

let apply = (model: Model.t, action, state, ~schedule_action): Model.t => {
  restart_caret_animation();
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
      let new_model = Update.apply(model, action, state, ~schedule_action);
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
    // TODO(andrew): reinstate this history functionality
    print_endline(Update.Failure.show(FailedToPerform(err)));
    //{...model, history: ActionHistory.failure(err, model.history)};
    model;
  | Error(err) =>
    print_endline(Update.Failure.show(err));
    model;
  };
};

module App = {
  module Model = Model;
  module Action = Update;
  module State = State;

  let on_startup = (~schedule_action, m: Model.t) => {
    let _ =
      observe_font_specimen("font-specimen", fm =>
        schedule_action(Haz3lweb.Update.SetMeta(FontMetrics(fm)))
      );

    JsUtil.focus_clipboard_shim();

    /* initialize state. */
    let state = State.init();

    /* Initial evaluation on a worker */
    Update.schedule_evaluation(~schedule_action, m);

    Os.is_mac :=
      Dom_html.window##.navigator##.platform##toUpperCase##indexOf(
        Js.string("MAC"),
      )
      >= 0;
    Async_kernel.Deferred.return(state);
  };

  let create =
      (
        model: Incr.t(Haz3lweb.Model.t),
        ~old_model as _: Incr.t(Haz3lweb.Model.t),
        ~inject,
      ) => {
    open Incr.Let_syntax;
    let%map model = model;
    /* Note: mapping over the old_model here may
       trigger an additional redraw */
    Component.create(
      ~apply_action=apply(model),
      model,
      Haz3lweb.Page.view(~inject, model),
      ~on_display=(_, ~schedule_action) => {
        if (edit_action_applied^
            && JsUtil.timestamp()
            -. last_edit_action^ > 1000.0) {
          /* If an edit action has been applied, but no other edit action
             has been applied for 1 second, save the model. */
          edit_action_applied := false;
          print_endline("Saving...");
          schedule_action(Update.Save);
        };
        if (scroll_to_caret.contents) {
          scroll_to_caret := false;
          JsUtil.scroll_cursor_into_view_if_needed();
        };
      },
    );
  };
};

switch (JsUtil.Fragment.get_current()) {
| Some("debug") => DebugMode.go()
| _ =>
  Incr_dom.Start_app.start(
    (module App),
    ~debug=false,
    ~bind_to_element_with_id="container",
    ~initial_model=Model.load(Model.blank),
  )
};
