open Util;
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
      model: Page.Model.t,
      action: Page.Update.t,
      ~schedule_action,
      ~schedule_autosave,
      ~schedule_undo,
      ~schedule_redo,
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
        ~schedule_action,
        ~schedule_undo,
        ~schedule_redo,
        action,
        model,
      )
    ) {
    | Haz3lcore.Action.Failure.Exception(t) =>
      Printf.printf(
        "ERROR: Action.Failure: %s\n",
        t |> Haz3lcore.Action.Failure.show,
      );
      model |> Updated.return_quiet;
    | exc =>
      Printf.printf(
        "ERROR: Exception during apply: %s\n",
        Printexc.to_string(exc),
      );
      model |> Updated.return_quiet;
    };
  // ---------- CALCULATE PHASE ----------
  let model' =
    updated.model
    |> Page.Update.calculate(
         ~schedule_action,
         ~is_edited=updated.is_edit,
         ~dynamics=true,
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

let start = {
  let%sub save_scheduler = BonsaiUtil.Alarm.alarm;
  let%sub undo_scope = BonsaiUndo.create_scope();
  {
    let%sub (app_model, app_inject) =
      BonsaiUndo.state_machine_with_undo(
        (module Page.Model),
        (module Page.Update),
        ~apply_action=
          (~inject, ~schedule_event, input) => {
            let schedule_action = x => schedule_event(inject(x));
            let schedule_autosave = action =>
              switch (input) {
              | Active(((_, alarm_inject), _)) =>
                schedule_event(alarm_inject(action))
              | Inactive => ()
              };
            let undo_inject = x =>
              switch (input) {
              | Active((_, (_, undo_inject))) => undo_inject(x)
              | Inactive => Ui_effect.Ignore
              };
            apply(
              ~schedule_action,
              ~schedule_autosave,
              ~schedule_undo=
                () =>
                  schedule_event(
                    undo_inject(BonsaiUndo.UndoScope.Action.AddUndo),
                  ),
              ~schedule_redo=
                () =>
                  schedule_event(
                    undo_inject(BonsaiUndo.UndoScope.Action.AddRedo),
                  ),
            );
          },
        ~default_model=
          Page.Store.load()
          |> Page.Update.calculate(
               ~schedule_action=_ => (),
               ~is_edited=true,
               ~dynamics=false,
             ),
        ~can_undo=Page.Update.can_undo,
        Bonsai.Value.both(save_scheduler, undo_scope),
      );

    // Autosave every second
    let save_effect =
      Bonsai.Value.map(~f=g => g(Page.Update.Save), app_inject);
    let%sub () = BonsaiUtil.Alarm.listen(save_scheduler, ~event=save_effect);

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
               i(
                 Page.Update.Globals(
                   SetFontMetrics({
                     row_height: rect.height,
                     col_width: rect.width,
                   }),
                 ),
               )
             }),
      );

    // Other Initialization
    let on_startup = (schedule_action, ()): unit => {
      Os.is_mac :=
        Dom_html.window##.navigator##.platform##toUpperCase##indexOf(
          Js.string("MAC"),
        )
        >= 0;
      NinjaKeys.initialize(Shortcut.options(schedule_action));
      JsUtil.focus_clipboard_shim();
      schedule_action(
        Assistant(AssistantUpdate.ChatAction(FilterLoadingMessages)),
      );
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
          model.globals.settings.core.statics ? Animation.go() : ();
        },
        (),
      );
    };
    let%sub () = Bonsai.Edge.after_display(after_display);

    // View function
    let%arr app_model = app_model
    and app_inject = app_inject;
    Page.View.view(app_model, ~inject=app_inject);
  }
  |> BonsaiUndo.set_scope(undo_scope);
};

// Set global undo scope
switch (JsUtil.Fragment.get_current()) {
| Some("debug") => DebugMode.go()
| _ => Bonsai_web.Start.start(start, ~bind_to_element_with_id="container")
};
