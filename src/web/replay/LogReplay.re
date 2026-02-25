/* Standalone Node.js script for replaying Hazel log files.
 *
 * Usage: node logReplay.js <log-file>
 *
 * Accepts either:
 *   - A JSON submission file (Export.all format)
 *   - A sexp replay log file (.hzlog.sexp) with optional initial state
 *
 * Replays each action, running statics/dynamics synchronously (no web worker),
 * printing each step. Stops on crash with error details. */

open Web;

/* Read a file to string (works in Node.js via js_of_ocaml) */
let read_file = (path: string): string => {
  let ic = open_in(path);
  let n = in_channel_length(ic);
  let s = Bytes.create(n);
  really_input(ic, s, 0, n);
  close_in(ic);
  Bytes.to_string(s);
};

/* Initialize model from optional initial state */
let init_state = (initial_state: option(Export.all)): History.Model.t =>
  switch (initial_state) {
  | Some(all) =>
    let json = Export.yojson_of_all(all) |> Yojson.Safe.to_string;
    Export.import_all(
      ~import_log=_ => (),
      json,
      ~exercise_specs=ExerciseSettings.exercises,
      ~tutorial_specs=TutorialSettings.lessons,
    );
    History.Model.load();
  | None => History.Model.reset()
  };

/* Actions that are handled at the Logged/CrashHandling level
 * and would failwith in Page.Update.update */
let should_skip = (action: Page.Update.t): bool =>
  switch (action) {
  | Globals(Log(_) | RethrowException | ClearException | InitImportAll(_)) =>
    true
  | _ => false
  };

/* Replay all actions */
let replay = (initial_model: History.Model.t, actions: Log.Entry.s) => {
  let model = ref(initial_model);
  let step = ref(0);

  let schedule_action = (_: Page.Update.t) => ();
  let import_log = (_: string) => ();
  let get_log_and = (f: string => unit) => f("");

  List.iter(
    ((timestamp, action)) => {
      incr(step);
      if (should_skip(action)) {
        Printf.printf(
          "Step %d (t=%.0f): SKIP %s\n%!",
          step^,
          timestamp,
          Page.Update.show(action),
        );
      } else {
        Printf.printf(
          "Step %d (t=%.0f): %s\n%!",
          step^,
          timestamp,
          Page.Update.show(action),
        );
        try({
          let updated =
            History.Update.update(
              ~import_log,
              ~get_log_and,
              ~schedule_action,
              action,
              model^,
            );
          let new_model = updated.model;
          let new_model =
            History.Update.calculate(
              ~schedule_action,
              ~is_edited=updated.is_edit,
              ~dynamics=new_model.current.globals.settings.core.dynamics,
              ~use_worker=false,
              new_model,
            );
          model := new_model;
        }) {
        | Updated.InvalidAction =>
          Printf.printf("  (InvalidAction - skipped)\n%!")
        | exn =>
          Printf.printf(
            "CRASH at step %d: %s\n%!",
            step^,
            Printexc.to_string(exn),
          );
          Printf.printf("Backtrace:\n%s\n%!", Printexc.get_backtrace());
          exit(1);
        };
      };
    },
    actions,
  );

  Printf.printf(
    "Replay complete: %d steps executed successfully.\n%!",
    step^,
  );
};

/* Main entry point */
let () = {
  Printexc.record_backtrace(true);

  if (Array.length(Sys.argv) < 2) {
    Printf.eprintf("Usage: node logReplay.js <log-file>\n%!");
    exit(1);
  };

  let path = Sys.argv[1];
  Printf.printf("Reading log file: %s\n%!", path);

  let data = read_file(path);
  let replay_log = ReplayLog.of_file(data);

  Printf.printf(
    "Parsed %d actions, initial_state: %s\n%!",
    List.length(replay_log.actions),
    switch (replay_log.initial_state) {
    | Some(_) => "present"
    | None => "none"
    },
  );

  let model = init_state(replay_log.initial_state);
  replay(model, replay_log.actions);
};
