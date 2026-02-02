open Util;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type state = Page.Model.t;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: state,
    undo_stack: list(Updated.t(state)),
    redo_stack: list(Updated.t(state)),
    future_log: list((float, Page.Update.t)),
    replay_toggle: bool,
  };

  let equal = (===);

  let init = () => {
    current: Page.Store.load(),
    undo_stack: [],
    redo_stack: [],
    future_log: [],
    replay_toggle: false,
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Page.Update.t;

  // let sexp = Page.Update.sexp_of_t(action);
  // For now, we don't ignore any actions; add here if needed
  // check if str contains "(Select (Term (Id" (ignoring whitespace)
  // let str = Sexplib.Sexp.to_string(sexp);
  // StringUtil.match(StringUtil.regexp("Select\\s*\\(Term\\s*\\(Id"), str);
  let ignore_if_action_fails_in_log_replay = (_action: t): bool => {
    false;
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  let update =
      (
        ~import_log,
        ~get_log_and,
        ~schedule_action: t => unit,
        action: t,
        model: Model.t,
      )
      : Updated.t(Model.t) =>
    switch (action) {
    | Globals(Undo) =>
      switch (model.undo_stack) {
      | [] =>
        print_endline("Cannot undo");
        model |> Updated.raise_invalid_action;
      | [x, ...rest] =>
        Log.Entry.save(Log.Entry.mk(action));
        {
          ...x,
          model: {
            current: x.model,
            future_log: model.future_log,
            undo_stack: rest,
            redo_stack: [
              {
                ...x,
                model: model.current,
              },
              ...model.redo_stack,
            ],
            replay_toggle: model.replay_toggle,
          },
        };
      }
    | Globals(Redo) =>
      switch (model.redo_stack) {
      | [] =>
        print_endline("Cannot redo");
        model |> Updated.raise_invalid_action;
      | [x, ...rest] =>
        Log.Entry.save(Log.Entry.mk(action));
        {
          ...x,
          model: {
            current: x.model,
            future_log: model.future_log,
            undo_stack: [
              {
                ...x,
                model: model.current,
              },
              ...model.undo_stack,
            ],
            redo_stack: rest,
            replay_toggle: model.replay_toggle,
          },
        };
      }
    | Globals(Log(a)) =>
      switch (a) {
      | InitImport(f) =>
        JsUtil.read_file(f, data =>
          schedule_action(Globals(Log(FinishImport(data))))
        );
        model |> Updated.return_quiet;
      | FinishImport(None) =>
        LogSidebar.log_error("Log import failed");
        model |> Updated.return_quiet;
      | FinishImport(Some(data)) =>
        let of_data = (data: string): list((float, Page.Update.t)) =>
          Export.import_just_log(data)
          |> Sexplib.Sexp.of_string
          |> Log.Entry.s_of_sexp_opt
          |> List.filter_map(x => x);
        let actions =
          data
          |> of_data
          |> Log.flatten_imports(~of_data)
          |> (
            x => {
              LogSidebar.log_info(
                "Imported log entries: " ++ string_of_int(List.length(x)),
              );
              x;
            }
          );
        {
          ...model,
          future_log: model.future_log @ actions,
        }
        |> Updated.return_quiet;
      | NextLog =>
        switch (model.future_log) {
        | [] =>
          LogSidebar.log_info("No next log action to perform");
          model |> Updated.return_quiet;
        | [(t, next), ...rest] =>
          LogSidebar.log_action(
            "Applying next log action",
            Some(JsUtil.print_timestamp(t)),
          );
          // Keep full action expression in console for detailed debugging
          print_endline("Full action: " ++ Page.Update.show(next));
          try({
            let updated =
              Page.Update.update(
                ~import_log,
                ~get_log_and,
                ~schedule_action,
                next,
                model.current,
              );
            {
              ...updated,
              model: {
                current: updated.model,
                undo_stack: [
                  {
                    ...updated,
                    model: model.current,
                  },
                  ...model.undo_stack,
                ],
                redo_stack: model.redo_stack,
                future_log: rest,
                replay_toggle: model.replay_toggle,
              },
            };
          }) {
          | _ =>
            LogSidebar.log_error("Failed to apply log action");
            Model.{
              ...model,
              future_log:
                ignore_if_action_fails_in_log_replay(next)
                  ? rest : model.future_log,
              replay_toggle:
                ignore_if_action_fails_in_log_replay(next)
                  ? model.replay_toggle : false,
            }
            |> Updated.return_quiet;
          };
        }
      | SkipLog =>
        LogSidebar.log_action("Skipping the next log entry", None);
        switch (model.future_log) {
        | [] =>
          LogSidebar.log_info("No log entry to skip");
          model |> return_quiet;
        | [(_, _), ...rest] =>
          {
            ...model,
            future_log: rest,
          }
          |> return_quiet
        };
      | SkipExercise =>
        LogSidebar.log_action(
          "Skipping to the next exercise in the log",
          None,
        );
        let rec skip_to_next_exercise = (log: list((float, Page.Update.t))) =>
          switch (log) {
          | [] => []
          | [(_, Editors(SwitchMode(_))), ..._] as rest => rest
          | [(_, Editors(Exercises(SwitchExercise(_)))), ..._] as rest => rest
          | [(_, _), ...rest] => skip_to_next_exercise(rest)
          };
        {
          ...model,
          future_log: skip_to_next_exercise(model.future_log),
        }
        |> return_quiet;
      | ToggleReplay =>
        Model.{
          ...model,
          replay_toggle: !model.replay_toggle,
        }
        |> return_quiet
      | ClearLog =>
        Log.DB.clear_and(() => ());
        LogSidebar.log_info("Log cleared");
        model |> return_quiet;
      }
    | action =>
      let current =
        Page.Update.update(
          ~import_log,
          ~get_log_and,
          ~schedule_action,
          action,
          model.current,
        );
      let _ = Log.update(action, current);
      if (Page.Update.can_undo(action)) {
        {
          ...current,
          model: {
            current: current.model,
            future_log: model.future_log,
            undo_stack: [
              {
                ...current,
                model: model.current,
              },
              ...model.undo_stack,
            ],
            redo_stack: [],
            replay_toggle: model.replay_toggle,
          },
        };
      } else {
        {
          ...current,
          model: {
            current: current.model,
            undo_stack: model.undo_stack,
            redo_stack: model.redo_stack,
            future_log: model.future_log,
            replay_toggle: model.replay_toggle,
          },
        };
      };
    };

  let calculate =
      (
        ~schedule_action: t => unit,
        ~is_edited: bool,
        ~dynamics,
        model: Model.t,
      )
      : Model.t => {
    current:
      model.current
      |> Page.Update.calculate(~schedule_action, ~is_edited, ~dynamics),
    undo_stack: model.undo_stack,
    redo_stack: model.redo_stack,
    future_log: model.future_log,
    replay_toggle: model.replay_toggle,
  };
};

module Selection = {
  type t = Page.selection;

  let handle_key_event = (model: Model.t) =>
    Page.Selection.handle_key_event(model.current);

  let get_cursor_info = (model: Model.t) =>
    Page.Selection.get_cursor_info(model.current);
};

module View = {
  let view =
      (~get_log_and, ~inject: Update.t => Ui_effect.t(unit), model: Model.t) => {
    Page.View.view(~get_log_and, ~inject, model.current);
  };
};
