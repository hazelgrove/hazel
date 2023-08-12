open Js_of_ocaml;
open Incr_dom;
open Haz3lweb;
open Virtual_dom.Vdom;

let action_applied = ref(true);
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

let apply = (model, action, state, ~schedule_action): Model.t => {
  restart_caret_animation();
  action_applied := true;
  if (UpdateAction.is_edit(action)) {
    last_edit_action := JsUtil.timestamp();
    edit_action_applied := true;
  };
  last_edit_action := JsUtil.timestamp();
  switch (
    try({
      let new_model = Update.apply(model, action, state, ~schedule_action);
      Log.update(action);
      new_model;
    }) {
    | exc => Error(Exception(Printexc.to_string(exc)))
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

let update_handler =
    (
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~model: Model.t,
      ~dir: Key.dir,
      evt: Js.t(Dom_html.keyboardEvent),
    )
    : Effect.t(unit) =>
  Effect.(
    switch (Keyboard.handle_key_event(Key.mk(dir, evt), ~model)) {
    | None => Ignore
    | Some(action) =>
      Many([Prevent_default, Stop_propagation, inject(action)])
    }
  );

let handlers =
    (~inject: UpdateAction.t => Ui_effect.t(unit), ~model: Model.t) => [
  Attr.on_keypress(_ => Effect.Prevent_default),
  Attr.on_keyup(update_handler(~inject, ~model, ~dir=KeyUp)),
  Attr.on_keydown(update_handler(~inject, ~model, ~dir=KeyDown)),
];

module App = {
  module Model = Model;
  module Action = Update;
  module State = State;

  let on_startup = (~schedule_action, _) => {
    let _ =
      observe_font_specimen("font-specimen", fm =>
        schedule_action(Haz3lweb.Update.SetMeta(FontMetrics(fm)))
      );

    JsUtil.focus_clipboard_shim();

    /* initialize state. */
    let state = State.init();

    /* create subscription to evaluator, updating model on each result. */
    let _ =
      State.evaluator_subscribe(
        state,
        ((key, r)) => {
          let cr: Haz3lcore.ModelResult.current =
            switch (r) {
            | Some(EvaluationOk(r)) => ResultOk(r)
            | Some(EvaluationFail(reason)) => ResultFail(reason)
            | None => ResultTimeout
            };
          schedule_action(Update.SetMeta(Result(key, cr)));
        },
        () => (),
      );

    Os.is_mac :=
      Dom_html.window##.navigator##.platform##toUpperCase##indexOf(
        Js.string("MAC"),
      )
      >= 0;

    switch (JsUtil.Fragment.get_current()) {
    | Some("auto") =>
      print_endline("AUTO: Starting script");
      schedule_action(SetMeta(Auto(StartRun())));
    | _ => ()
    };

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
      Haz3lweb.Page.view(~inject, ~handlers, model),
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
        if (action_applied.contents) {
          action_applied := false;
          JsUtil.scroll_cursor_into_view_if_needed();
        };
      },
    );
  };
};

let fragment =
  switch (JsUtil.Fragment.get_current()) {
  | None => ""
  | Some(frag) => frag
  };

let initial_model = {
  switch (fragment) {
  | "debug" => Model.debug
  | _ => Model.load(Model.blank)
  };
};

Incr_dom.Start_app.start(
  (module App),
  ~debug=false,
  ~bind_to_element_with_id="container",
  ~initial_model,
);
