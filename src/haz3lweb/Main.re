open Util;
open Js_of_ocaml;
open Haz3lweb;

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

let apply = (model, action, ~schedule_action): Model.t => {
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
      let new_model = Update.apply(model, action, (), ~schedule_action);
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

let on_startup =
    (~inject: UpdateAction.t => Ui_effect.t(unit), m: Model.t)
    : Ui_effect.t(unit) => {
  let _ =
    observe_font_specimen("font-specimen", fm =>
      schedule_action(Haz3lweb.Update.SetMeta(FontMetrics(fm)))
    );
  NinjaKeys.initialize(NinjaKeys.options(schedule_action));
  JsUtil.focus_clipboard_shim();
  /* initialize state. */
  /* Initial evaluation on a worker */
  Update.schedule_evaluation(~schedule_action, m);
  Os.is_mac :=
    Dom_html.window##.navigator##.platform##toUpperCase##indexOf(
      Js.string("MAC"),
    )
    >= 0;
  Ui_effect.Ignore;
};

module App = {
  module Model = Model;
  module Action = Update;
  module State = State;
  //   let create =
  //       (
  //         model: Incr.t(Haz3lweb.Model.t),
  //         ~old_model as _: Incr.t(Haz3lweb.Model.t),
  //         ~inject,
  //       ) => {
  //     open Incr.Let_syntax;
  //     let%map model = model;
  //     /* Note: mapping over the old_model here may
  //        trigger an additional redraw */
  //     Component.create(
  //       ~apply_action=apply(model),
  //       model,
  //       Haz3lweb.Page.view(~inject, model),
  //       ~on_display=(_, ~schedule_action) => {
  //         if (edit_action_applied^
  //             && JsUtil.timestamp()
  //             -. last_edit_action^ > 1000.0) {
  //           /* If an edit action has been applied, but no other edit action
  //              has been applied for 1 second, save the model. */
  //           edit_action_applied := false;
  //           print_endline("Saving...");
  //           schedule_action(Update.Save);
  //         };
  //         if (scroll_to_caret.contents) {
  //           scroll_to_caret := false;
  //           JsUtil.scroll_cursor_into_view_if_needed();
  //         };
  //       },
  //     );
  //   };
  // };
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

open Bonsai.Let_syntax;

let view = {
  let startup_completed = Bonsai.toggle'(~default_model=false);
  let%sub startup_completed = startup_completed;
  let%sub app = app;
  let%sub after_display = {
    switch%sub (startup_completed) {
    | {state: false, set_state, _} =>
      let%arr (model, inject) = app
      and set_state = set_state;
      Bonsai.Effect.Many([on_startup(~inject, model), set_state(true)]);
    | {state: true, _} => Bonsai.Computation.return(Ui_effect.Ignore)
    };
  };
  let%sub () = Bonsai.Edge.lifecycle(~after_display, ());
  let%arr (model, inject) = app;
  Haz3lweb.Page.view(~inject, model);
};

switch (JsUtil.Fragment.get_current()) {
| Some("debug") => DebugMode.go()
| _ => Bonsai_web.Start.start(view, ~bind_to_element_with_id="container")
};
