open Util;
open Js_of_ocaml;
open Incr_dom;
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

let apply =
    (
      model: Page.Model.t,
      action: Page.Update.t,
      _state: unit,
      ~schedule_action,
    )
    : Page.Model.t => {
  restart_caret_animation();

  /* This function is split into two phases, update and calculate.
     The intention is that eventually, the calculate phase will be
     done automatically by incremental calculation. */
  // ---------- UPDATE PHASE ----------
  let updated: Updated.t(Page.Model.t) =
    try(
      Page.Update.update(
        ~import_log=Log.import,
        ~schedule_action,
        action,
        model,
      )
    ) {
    | exc =>
      Printf.printf(
        "ERROR: Exception during apply: %s\n",
        Printexc.to_string(exc),
      );
      model |> Updated.return_quiet;
    };
  // ---------- CALCULATE PHASE ----------
  let model' =
    updated.recalculate
      ? updated.model
        |> Page.Update.calculate(~schedule_action, ~is_edited=updated.is_edit)
      : updated.model;

  if (updated.is_edit) {
    last_edit_action := JsUtil.timestamp();
    edit_action_applied := true;
  };
  if (updated.scroll_active) {
    scroll_to_caret := true;
  };
  model';
};

module App = {
  module Model = Page.Model;
  module Action = Page.Update;
  module State = {
    type t = unit;
    let init = () => ();
  };

  let on_startup = (~schedule_action, _: Model.t) => {
    let _ =
      observe_font_specimen("font-specimen", fm =>
        schedule_action(Haz3lweb.Page.Update.Globals(SetFontMetrics(fm)))
      );

    NinjaKeys.initialize(NinjaKeys.options(schedule_action));
    JsUtil.focus_clipboard_shim();

    Js.Unsafe.set(
      Js.Unsafe.global##._Error,
      "stackTraceLimit",
      Js.number_of_float(infinity),
    );

    /* initialize state. */
    let state = State.init();

    schedule_action(Start);

    Os.is_mac :=
      Dom_html.window##.navigator##.platform##toUpperCase##indexOf(
        Js.string("MAC"),
      )
      >= 0;
    Async_kernel.Deferred.return(state);
  };

  let create =
      (model: Incr.t(Model.t), ~old_model as _: Incr.t(Model.t), ~inject) => {
    open Incr.Let_syntax;
    let%map model = model;
    /* Note: mapping over the old_model here may
       trigger an additional redraw */
    Component.create(
      ~apply_action=apply(model),
      model,
      Haz3lweb.Page.View.view(~get_log_and=Log.get_and, ~inject, model),
      ~on_display=(_, ~schedule_action) => {
        if (edit_action_applied^
            && JsUtil.timestamp()
            -. last_edit_action^ > 1000.0) {
          /* If an edit action has been applied, but no other edit action
             has been applied for 1 second, save the model. */
          edit_action_applied := false;
          print_endline("Saving...");
          schedule_action(Page.Update.Save);
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
    ~initial_model=Page.Store.load(),
  )
};
