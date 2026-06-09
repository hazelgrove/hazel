open Util;
open Js_of_ocaml;
open Web;
open Bonsai.Let_syntax;

let scroll_to_caret = ref(true);

/* Inputs the published #main scroll width depends on, captured on the last
 * re-measure. update_main_scroll_width forces two whole-document layouts
 * (write-read-write on a :root CSS var), so after_display only re-measures
 * when one of these changes: syntax measurements / drawer heights (by
 * reference), probe display state (Settings.version covers sample lengths,
 * window mode, dropdowns), sample focus (changes which samples render),
 * font metrics and viewport width (px scaling). */
let scroll_width_key = ref(None);

/* Per-slide scroll memory for tutorial mode. Each slide remembers where the
   user last left it; revisiting a slide restores that scroll position, while
   a slide that's never been scrolled opens at the top. */
let slide_scrolls: ref(list((int, float))) = ref([]);
let pending_scroll_restore: ref(option(float)) = ref(None);

/* The current tutorial slide index, if the app is in tutorial mode. */
let tutorial_slide = (m: CrashHandling.Model.t): option(int) =>
  switch (m.model.current.current.editors) {
  | Editors.Model.Tutorial(tm) => Some(tm.current)
  | _ => None
  };

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

/* Seed the viewport-culling row range the first frame it's needed (auto-probe
   on, single-code-editor mode), so culling activates on load rather than only
   after the first scroll. Reads the DOM only while visible_rows is None, so it
   adds no per-frame forced layout; ongoing updates come from Page.on_scroll.
   Measured against the active editor's local code container — see
   JsUtil.code_viewport_geometry. */
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
  /* When the tutorial slide changes, stash the outgoing slide's scroll
     position and queue a restore of the incoming slide's saved position
     (the top, for slides that have never been scrolled). The restore takes
     precedence over scroll-to-caret so a fresh slide opens at its prompt. */
  switch (tutorial_slide(model), tutorial_slide(updated.model)) {
  | (Some(prev), Some(next)) when prev != next =>
    slide_scrolls :=
      [
        (prev, ScrollDebug.main_scroll_top()),
        ...List.remove_assoc(prev, slide_scrolls^),
      ];
    pending_scroll_restore :=
      Some(
        List.assoc_opt(next, slide_scrolls^) |> Option.value(~default=0.),
      );
    scroll_to_caret := false;
  | _ => ()
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
    let%map model = app_model
    and app_inject = app_inject;
    Bonsai.Effect.of_sync_fun(
      () => {
        ScrollDebug.next_frame();
        /* Drift detection only during EdgeScroll-active periods (drag at
         * edge); otherwise wheel-scroll would flood the log. */
        ScrollDebug.check_drift(~in_drag=EdgeScroll.is_active(), ());
        if (scroll_to_caret.contents) {
          ScrollDebug.log(
            "AF",
            Printf.sprintf(
              "frame_start sT=%.1f scroll_to_caret=t",
              ScrollDebug.main_scroll_top(),
            ),
          );
        };
        if (scroll_to_caret.contents) {
          scroll_to_caret := false;
          JsUtil.scroll_cursor_into_view_if_needed();
        } else {
          ();
        };
        /* Tutorial slide switch: restore the incoming slide's remembered
         * scroll position (top for never-scrolled slides). Runs after
         * scroll_to_caret so the restore wins if both somehow fire. */
        switch (pending_scroll_restore^) {
        | Some(target) =>
          pending_scroll_restore := None;
          JsUtil.set_main_scroll_top(target);
        | None => ()
        };
        /* Handle scheduled probe focus from step-into (see ProbePerform.FocusEffect) */
        let _ = Haz3lcore.ProbePerform.FocusEffect.execute();
        /* Scroll-compensate when focus bar appears/disappears */
        JsUtil.setup_focus_bar_scroll_compensation();
        /* Update floating elements (backpack) to viewport coordinates */
        FloatingElement.update_all();
        let editor =
          Page.Update.get_editor(model.model.current.current).editor;
        let zipper = editor.state.zipper;
        let measured = editor.syntax.measured;
        let font_metrics = model.model.current.current.globals.font_metrics;
        /* Publish #main's effective scroll width (including absolutely-
         * positioned probe overlays / drawers) so .cell can stretch its
         * background to match. CSS-only intrinsic sizing can't see
         * absolute descendants, so we measure here — but only when an
         * input it depends on changed (see scroll_width_key): the
         * measurement itself forces two full-document layouts, which
         * scales with probe count and used to run on every frame. */
        let viewport_w = Dom_html.document##.documentElement##.clientWidth;
        let key = (
          measured,
          editor.syntax.refractor_shape_map,
          Haz3lcore.ProbeProj.Settings.version^,
          zipper.refractors.sample_focus,
          font_metrics,
          viewport_w,
        );
        let scroll_width_stale =
          switch (scroll_width_key^) {
          | Some((m, rsm, v, sf, fm, w)) =>
            m !== measured
            || rsm !== editor.syntax.refractor_shape_map
            || v != Haz3lcore.ProbeProj.Settings.version^
            || sf != zipper.refractors.sample_focus
            || fm != font_metrics
            || w != viewport_w
          | None => true
          };
        if (scroll_width_stale) {
          JsUtil.update_main_scroll_width();
          scroll_width_key := Some(key);
        };
        /* Cause-driven refractor-shift compensation: when a drawer
         * above the caret changes height, scroll #main by the exact
         * pixel delta so the caret row stays put. Compensation is
         * gated by `refractor_shape_map` reference identity, so idle
         * frames and refractor-irrelevant edits do zero work. */
        RefractorShift.update(
          ~font_metrics,
          ~refractor_shape_map=editor.syntax.refractor_shape_map,
          ~measured,
          zipper,
        );
        ScrollDebug.mark_sT();
        /* Sample-focus anchor compensation: if Left/Right in the sample
         * focus bar captured the indicated sample's screen-y before
         * dispatch, restore it now so the user's eye stays on it. */
        SampleAnchor.consume();
        ScrollDebug.mark_sT();
        seed_visible_rows(model, ~dispatch=a =>
          app_inject(a) |> Bonsai.Effect.Expert.handle
        );
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
