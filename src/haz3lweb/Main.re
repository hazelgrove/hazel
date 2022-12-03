open Js_of_ocaml;
open Incr_dom;
open Haz3lweb;

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
  | Error(UnrecognizedInput(reason)) =>
    // TODO(andrew): reinstate this history functionality
    print_endline(Update.Failure.show(UnrecognizedInput(reason)));
    model;
  //{...model, history: ActionHistory.just_failed(reason, model.history)};
  | Error(err) =>
    print_endline(Update.Failure.show(err));
    model;
  };
};

let do_many = (evts): Virtual_dom.Vdom.Effect.t(unit) => {
  Virtual_dom.Vdom.Effect.(
    switch (evts) {
    | [] => Many([])
    | evts => Many([Prevent_default, Stop_propagation, ...evts])
    }
  );
};

let update_handler = (~inject, ~model, ~dir: Key.dir, evt) => {
  let key = Key.mk(dir, evt);
  Keyboard.handle_key_event(key, ~model) |> List.map(inject) |> do_many;
};

let handlers = (~inject, ~model: Model.t) =>
  Virtual_dom.Vdom.[
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
        schedule_action(Haz3lweb.Update.SetFontMetrics(fm))
      );

    JsUtil.focus_clipboard_shim();

    /* initialize state. */
    let state = State.init();

    /* create subscription to evaluator, updating model on each result. */
    let _ =
      State.evaluator_subscribe(
        state,
        ((key, r)) => {
          let cr: ModelResult.current =
            switch (r) {
            | Some(EvaluationOk(r)) => ResultOk(r)
            | Some(EvaluationFail(reason)) => ResultFail(reason)
            | None => ResultTimeout
            };
          schedule_action(Update.UpdateResult(key, cr));
        },
        () => (),
      );

    Os.is_mac :=
      Dom_html.window##.navigator##.platform##toUpperCase##indexOf(
        Js.string("MAC"),
      )
      >= 0;

    Async_kernel.Deferred.return(state);
  };

  let create = (model: Incr.t(Haz3lweb.Model.t), ~old_model as _, ~inject) => {
    open Incr.Let_syntax;
    let%map model = model;
    Component.create(
      ~apply_action=apply(model),
      model,
      Haz3lweb.Page.view(~inject, ~handlers, model),
    );
  };
};

let fragment =
  switch (JsUtil.Fragment.get_current()) {
  | None => ""
  | Some(frag) => frag
  };

let initial_model = {
  // NOTE: load settings first to get last editor mode
  let model = Update.load_model(Model.blank);
  switch (fragment) {
  | "dynamics-off" =>
    print_endline("Turning off dynamics...");
    let settings = {...model.settings, dynamics: false};
    LocalStorage.Settings.save(settings);
    let model = {...model, settings};
    model;
  | _ => model
  };
};

Incr_dom.Start_app.start(
  (module App),
  ~debug=false,
  ~bind_to_element_with_id="container",
  ~initial_model,
);
