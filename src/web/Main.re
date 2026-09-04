open Util_web;
open Js_of_ocaml;
open Web;
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

let apply =
    (
      model: CrashHandling.Model.t,
      action: CrashHandling.Update.t,
      ~schedule_action,
      ~schedule_autosave,
    )
    : CrashHandling.Model.t => {
  restart_caret_animation();

  /* This function is split into two phases, update and calculate.
     The intention is that eventually, the calculate phase will be
     done automatically by incremental calculation. */
  // ---------- UPDATE PHASE ----------
  let updated: Updated.t(CrashHandling.Model.t) =
    CrashHandling.Update.update(
      ~import_log=Log.import,
      ~get_log_and=Log.get_and,
      ~schedule_action,
      action,
      model,
    );
  // ---------- CALCULATE PHASE ----------
  let model' =
    CrashHandling.Update.calculate(
      ~schedule_action,
      ~is_edited=updated.is_edit,
      ~dynamics=true,
      model,
      updated.model,
    );

  if (updated.is_edit) {
    schedule_autosave(
      BonsaiUtil.Alarm.Action.SetAlarm(
        Core.Time_ns.add(Core.Time_ns.now(), Core.Time_ns.Span.of_sec(1.0)),
      ),
    );
  } else {
    schedule_autosave(
      BonsaiUtil.Alarm.Action.SnoozeAlarm(
        Core.Time_ns.add(Core.Time_ns.now(), Core.Time_ns.Span.of_sec(1.0)),
      ),
    );
  };
  if (updated.scroll_active) {
    scroll_to_caret := true;
  };
  model';
};

let start = default_model => {
  let%sub save_scheduler = BonsaiUtil.Alarm.alarm;
  let%sub (app_model, app_inject) =
    Bonsai.state_machine1(
      (module CrashHandling.Model),
      (module CrashHandling.Update),
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
      ~default_model,
      save_scheduler,
    );

  // Autosave every second
  let save_effect =
    Bonsai.Value.map(~f=g => g(Page.Update.Save), app_inject);
  let%sub () = BonsaiUtil.Alarm.listen(save_scheduler, ~event=save_effect);

  let replay_effect = {
    let%map app_inject = app_inject
    and model = app_model;
    Ui_effect.Many(
      model.model.replay_toggle
        ? [app_inject(Page.Update.Globals(Log(NextLog)))] : [],
    );
  };

  let%sub () =
    Bonsai.Clock.every(
      ~when_to_start_next_effect=`Wait_period_after_previous_effect_finishes_blocking,
      Core.Time_ns.Span.of_sec(0.1),
      replay_effect,
    );

  // Update font metrics on resize
  let%sub size =
    BonsaiUtil.SizeObserver.observer(
      () => JsUtil.get_elem_by_id("font-specimen"),
      ~default=
        BonsaiUtil.SizeObserver.Size.{
          width: 10.,
          height: 10.,
        },
    );
  let%sub () =
    /* Note: once Bonsai is threaded through the system, we won't need
       on_change here */
    Bonsai.Edge.on_change(
      (module BonsaiUtil.SizeObserver.Size),
      size,
      ~callback=
        app_inject
        |> Bonsai.Value.map(~f=(i, rect: BonsaiUtil.SizeObserver.Size.t) => {
             JsUtil.set_css_custom_property(
               "--row-height-px",
               Printf.sprintf("%fpx", rect.height),
             );
             i(
               Page.Update.Globals(
                 SetFontMetrics({
                   row_height: rect.height,
                   col_width: rect.width,
                 }),
               ),
             );
           }),
    );

  // Other Initialization
  let on_startup = (schedule_action, ()): unit => {
    Os.is_mac :=
      Dom_html.window##.navigator##.platform##toUpperCase##indexOf(
        Js.string("MAC"),
      )
      >= 0;
    JsUtil.focus_clipboard_shim();
    /* Re-measure font metrics on zoom (DPR change). ResizeObserver
     * doesn't fire on zoom because CSS-level dimensions don't change,
     * but getBoundingClientRect returns different values due to
     * device-pixel rounding at different zoom levels. */
    JsUtil.on_dpr_change(() => {
      let (col_width, row_height) = JsUtil.font_metrics_from_specimen();
      schedule_action(
        Page.Update.Globals(
          SetFontMetrics({
            row_height,
            col_width,
          }),
        ),
      );
    });
    /* Setup scroll listener for floating elements (backpack) */
    FloatingElement.setup_scroll_listener();
    // Sync log count from database
    Log.sync_count();
  };
  let%sub () =
    BonsaiUtil.OnStartup.on_startup(
      {
        let%map app_inject = app_inject;
        Bonsai.Effect.Many([
          // Initialize state
          Bonsai.Effect.of_sync_fun(
            on_startup(x => x |> app_inject |> Bonsai.Effect.Expert.handle),
            (),
          ),
          // Initialize evaluation on a worker
          app_inject(Start),
        ]);
      },
    );

  // Triggers after every update
  let after_display = {
    let%map model = app_model;
    Bonsai.Effect.of_sync_fun(
      () => {
        if (scroll_to_caret.contents) {
          scroll_to_caret := false;
          JsUtil.scroll_cursor_into_view_if_needed();
        } else {
          ();
        };
        /* Handle scheduled probe focus from step-into (see ProbePerform.FocusEffect) */
        let _ = Haz3lcore.ProbePerform.FocusEffect.execute();
        /* Scroll-compensate when focus bar appears/disappears */
        JsUtil.setup_focus_bar_scroll_compensation();
        /* Update floating elements (backpack) to viewport coordinates */
        FloatingElement.update_all();
        model.model.current.current.globals.settings.core.statics
          ? Animation.go() : ();
      },
      (),
    );
  };
  let%sub () = Bonsai.Edge.after_display(after_display);

  // View function
  let%arr app_model = app_model
  and app_inject = app_inject;
  try(
    CrashHandling.View.view(
      ~get_log_and=Log.get_and,
      ~inject=app_inject,
      app_model,
    )
  ) {
  | exc =>
    print_endline(
      "ERROR: Exception during view: " ++ Printexc.to_string(exc),
    );
    WebUtil.Node.div(
      ~attrs=[WebUtil.Attr.id("page")],
      [WebUtil.Node.text("An error occurred.")],
    );
  };
};

switch (JsUtil.Fragment.get_current()) {
| Some("debug") => DebugMode.go()
| _ =>
  /* Load all IndexedDB data, then construct model and start Bonsai.
     The hazelnut loading spinner (in index.html) stays visible until
     Bonsai renders its first frame. */
  HazelDB.kv_load_all(_pairs => {
    let model = CrashHandling.Model.load();
    let default_model =
      CrashHandling.Update.calculate(
        ~schedule_action=_ => (),
        ~is_edited=true,
        ~dynamics=false,
        model,
        model,
      );
    Bonsai_web.Start.start(
      start(default_model),
      ~bind_to_element_with_id="container",
    );
  })
};
