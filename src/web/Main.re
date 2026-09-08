open Util;
open Js_of_ocaml;
open Web;
open Bonsai.Let_syntax;

let scroll_to_caret = ref(true);

/* console: window.__incrCounters() — MakeTerm.Incr observability
   (fell_back should stay 0; analyzed ~1 per stacked edit) */
/* console: window.__normCounters() — sparse remold/regrout regime
   observability (fallbacks fire on structure-entering edits; a hot
   fallback rate is the "forgotten spike" signal, ledger §17) */
let () =
  Js_of_ocaml.Js.Unsafe.set(
    Js_of_ocaml.Js.Unsafe.global,
    "__normCounters",
    Js_of_ocaml.Js.wrap_callback(() =>
      Js_of_ocaml.Js.string(
        Printf.sprintf(
          "sparse_hits=%d sparse_fallbacks=%d",
          Haz3lcore.Zipper.sparse_hits^,
          Haz3lcore.Zipper.sparse_fallbacks^,
        ),
      )
    ),
  );
let () =
  Js_of_ocaml.Js.Unsafe.set(
    Js_of_ocaml.Js.Unsafe.global,
    "__incrCountersReset",
    Js_of_ocaml.Js.wrap_callback(() => {
      Haz3lcore.MakeTerm.Incr.fell_back := 0;
      Haz3lcore.MakeTerm.Incr.full_analyzed := 0;
      Haz3lcore.MakeTerm.Incr.analyzed := 0;
      Haz3lcore.MakeTerm.Incr.incr_calls := 0;
      Haz3lcore.MakeTerm.Incr.incr_hits := 0;
      Haz3lcore.MakeTerm.Incr.incr_misses := 0;
    }),
  );
let () =
  Js_of_ocaml.Js.Unsafe.set(
    Js_of_ocaml.Js.Unsafe.global,
    "__incrCounters",
    Js_of_ocaml.Js.wrap_callback(() =>
      Js_of_ocaml.Js.string(
        Printf.sprintf(
          "fell_back=%d full_analyzed=%d analyzed=%d calls=%d hits=%d misses=%d neq=%d nokey=%d",
          Haz3lcore.MakeTerm.Incr.fell_back^,
          Haz3lcore.MakeTerm.Incr.full_analyzed^,
          Haz3lcore.MakeTerm.Incr.analyzed^,
          Haz3lcore.MakeTerm.Incr.incr_calls^,
          Haz3lcore.MakeTerm.Incr.incr_hits^,
          Haz3lcore.MakeTerm.Incr.incr_misses^,
          Haz3lcore.MakeTerm.Incr.incr_miss_neq^,
          Haz3lcore.MakeTerm.Incr.incr_miss_nokey^,
        ),
      )
    ),
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

/* Seed the culling range on the first frame it's needed, so culling activates
   on load rather than only after the first scroll. Reads the DOM only while
   visible_rows is None, so it adds no per-frame layout. */
let seed_visible_rows =
    (model: CrashHandling.Model.t, ~dispatch: Page.Update.t => unit): unit => {
  let page = model.model.current.current;
  let needed =
    Editors.Model.supports_viewport_culling(page.editors)
    && page.globals.settings.autoprobe_mode != Haz3lcore.AutoProbe.Off
    && Option.is_none(page.globals.visible_rows);
  if (needed) {
    switch (JsUtil.code_viewport_geometry()) {
    | None => ()
    | Some((scroll_top, client_height)) =>
      dispatch(
        Page.Update.Globals(
          UpdateVisibleRows(
            Globals.VisibleRows.compute(
              ~scroll_top,
              ~client_height,
              ~row_height=page.globals.font_metrics.row_height,
              (),
            ),
          ),
        ),
      )
    };
  };
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
  /* the action's constructor path, by hand: serializing the whole
     action (its sexp) hung the page on evaluation results — closure
     environments in streamed values serialize exponentially */
  let kind = (action: CrashHandling.Update.t): string =>
    switch (action) {
    | Globals(Set(CanvasTick)) => "Globals/Set/CanvasTick"
    | Globals(Set(Sidebar(_))) => "Globals/Set/Sidebar"
    | Globals(Set(_)) => "Globals/Set"
    | Globals(ActiveEditor(_)) => "Globals/ActiveEditor"
    | Globals(SelectTile(_) | JumpToTile(_)) => "Globals/Jump"
    | Globals(SetAgentGlobals(_)) => "Globals/SetAgentGlobals"
    | Globals(AppViewMsg(_)) => "Globals/AppViewMsg"
    | Globals(Undo | Redo) => "Globals/Undo"
    | Globals(_) => "Globals/other"
    | Editors(Scratch(CellAction(MainEditor(_)))) => "Editors/CellAction/MainEditor"
    | Editors(Scratch(CellAction(ResultAction(_)))) => "Editors/CellAction/Result"
    | Editors(Scratch(StackBody(_) | StackHeader(_))) => "Editors/Stack"
    | Editors(Scratch(AgentAction(_))) => "Editors/AgentAction"
    | Editors(
        Scratch(FocusDef(_) | FocusToggle(_) | FocusEnsure(_) | UnfocusDef),
      ) => "Editors/Focus"
    | Editors(Scratch(_)) => "Editors/Scratch/other"
    | Editors(_) => "Editors/other"
    | ExplainThis(_) => "ExplainThis"
    | MakeActive(_) => "MakeActive"
    | Benchmark(_) => "Benchmark"
    | Refresh => "Refresh"
    | Start => "Start"
    | Save => "Save"
    };
  let t_upd = Util.PerfTimer.now();
  let updated: Updated.t(CrashHandling.Model.t) =
    Util.PerfTimer.time("app/update", () =>
      CrashHandling.Update.update(
        ~import_log=Log.import,
        ~get_log_and=Log.get_and,
        ~schedule_action,
        action,
        model,
      )
    );
  if (Util.PerfTimer.now() -. t_upd > 100.) {
    Util.PerfTimer.record("slow-update/" ++ kind(action), 0.);
  };
  /* every action, by kind: the perf journal's re-render census (a score
     ran the app at 4 Hz with the agent idle — who was ticking?) */
  Util.PerfTimer.record("action/" ++ kind(action), 0.);
  /* which actions count as edits (each one costs a statics/eval recompute):
     the perf journal names them */
  if (updated.is_edit) {
    Util.PerfTimer.record("edit-action/" ++ kind(action), 0.);
  };
  // ---------- CALCULATE PHASE ----------
  let t_calc = Util.PerfTimer.now();
  let model' =
    Util.PerfTimer.time("app/calculate", () =>
      CrashHandling.Update.calculate(
        ~schedule_action,
        ~is_edited=updated.is_edit,
        ~dynamics=true,
        model,
        updated.model,
      )
    );
  /* a calculate phase over 100 ms is a stall the journal should name by
     its action (eval results landing, agent tool results, edits) */
  if (Util.PerfTimer.now() -. t_calc > 100.) {
    Util.PerfTimer.record("slow-calc/" ++ kind(action), 0.);
  };

  if (updated.save) {
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

/* route core parse-fallback telemetry into the constellation journal */
Haz3lcore.CompositionGo.fallback_notice := Some(CanvasLog.log);

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
    /* canvas trajectory replay: a recorded reply's tool calls go through
       the real agent handler; the agent reads as busy for the avatar */
    CanvasTrajectory.dispatch_reply :=
      (
        calls =>
          schedule_action(
            Page.Update.Editors(
              Editors.Update.Scratch(
                ScratchMode.Update.AgentAction(
                  Agent.Update.Action.ReplayToolCalls(
                    List.mapi(
                      (i, (name, args)) =>
                        OpenRouter.Reply.Model.{
                          id: "replay-" ++ string_of_int(i),
                          name,
                          args,
                        },
                      calls,
                    ),
                  ),
                ),
              ),
            ),
          )
      );
    CanvasTrajectory.dispatch_begin :=
      (
        label =>
          schedule_action(
            Page.Update.Editors(
              Editors.Update.Scratch(
                ScratchMode.Update.AgentAction(
                  Agent.Update.Action.ReplayBegin(label),
                ),
              ),
            ),
          )
      );
    CanvasTrajectory.dispatch_new_slide :=
      (
        () =>
          schedule_action(
            Page.Update.Editors(
              Editors.Update.Scratch(ScratchMode.Update.AddSlide),
            ),
          )
      );
    CanvasTrajectory.dispatch_paste :=
      (
        text =>
          schedule_action(
            Page.Update.Editors(
              Editors.Update.Scratch(
                ScratchMode.Update.CellAction(
                  CellEditor.Update.MainEditor(
                    CodeEditable.Update.Perform(
                      Haz3lcore.Action.Paste(text),
                    ),
                  ),
                ),
              ),
            ),
          )
      );
    CanvasTrajectory.on_change :=
      (
        () =>
          schedule_action(
            Page.Update.Globals(
              Globals.Update.Set(Settings.Update.CanvasTick),
            ),
          )
      );
    CanvasTrajectory.dispatch_tick :=
      (
        () =>
          schedule_action(
            Page.Update.Editors(
              Editors.Update.Scratch(
                ScratchMode.Update.AgentAction(
                  Agent.Update.Action.ReplayStreamTick,
                ),
              ),
            ),
          )
      );
    CanvasTrajectory.set_busy :=
      (b => CanvasBuffer.fake_busy_until := b ? CanvasBuffer.now() +. 1e9 : 0.);
    CanvasTrajectory.install_testers();
    Animation.slow_hook := CanvasLog.log;
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
    let%map model = app_model
    and app_inject = app_inject;
    Bonsai.Effect.of_sync_fun(
      () => {
        if (scroll_to_caret.contents) {
          scroll_to_caret := false;
          CaretReveal.reveal();
        } else {
          ();
        };
        let _ = Haz3lcore.FocusEffect.execute();
        /* restore probe focus dropped by vdom reorder moves */
        Haz3lcore.FocusEffect.keep_focus();
        /* Scroll-compensate when focus bar appears/disappears */
        JsUtil.setup_focus_bar_scroll_compensation();
        /* Update floating elements (backpack) to viewport coordinates */
        FloatingElement.update_all();
        let editor =
          Page.Update.get_editor(model.model.current.current).editor;
        let zipper = editor.state.zipper;
        let measured = editor.syntax.measured;
        let font_metrics = model.model.current.current.globals.font_metrics;
        RefractorShift.update(
          ~editor_key=
            Editors.Model.editor_key(model.model.current.current.editors),
          ~font_metrics,
          ~refractor_rows=editor.syntax.refractor_rows,
          ~measured,
          zipper,
        );
        /* stagger multi-row offside displays clear of code and of each
           other (top-down priority, first-fit), per code container */
        ProbeStagger.update(~font_metrics);
        /* measure AFTER the shift/stagger patches so the published scroll
           width includes displays pushed right by staggering */
        ScrollWidth.update(
          ~measured,
          ~refractor_rows=editor.syntax.refractor_rows,
          ~sample_focus=zipper.refractors.sample_focus,
          ~font_metrics,
          ~visible_rows=model.model.current.current.globals.visible_rows,
        );
        SampleAnchor.consume();
        seed_visible_rows(model, ~dispatch=a =>
          app_inject(a) |> Bonsai.Effect.Expert.handle
        );
        model.model.current.current.globals.settings.core.statics
          ? Animation.go() : ();
        /* Play any pending code-movement ghosts (see CodeFlip.re) */
        try({
          let page = model.model.current.current;
          let syntax = Page.Update.get_editor(page).editor.syntax;
          IdWatch.check(syntax.segment);
          CodeFlip.go(~syntax, ~font_metrics=page.globals.font_metrics);
        }) {
        | _ => ()
        };
      },
      (),
    );
  };
  let%sub () = Bonsai.Edge.after_display(after_display);

  // View function
  let%arr app_model = app_model
  and app_inject = app_inject;
  try(
    Util.PerfTimer.time("app/view", () =>
      CrashHandling.View.view(
        ~get_log_and=Log.get_and,
        ~inject=app_inject,
        app_model,
      )
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
