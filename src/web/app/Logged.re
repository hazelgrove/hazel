open Util;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type state = History.Model.t;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: state,
    future_log: list((float, History.Update.t)),
    past_log: list((float, History.Update.t)),
    replay_messages: list(string),
    replay_toggle: bool,
  };

  let equal = (===);

  let load = () => {
    current: History.Model.load(),
    future_log: [],
    past_log: [],
    replay_messages: [],
    replay_toggle: false,
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = History.Update.t;

  // let sexp = History.Update.sexp_of_t(action);

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
        let of_data = (data: string): list((float, History.Update.t)) =>
          Export.import_just_log(data)
          |> Sexplib.Sexp.of_string
          |> Log.Entry.s_of_sexp_opt
          |> List.filter_map(x => x);
        let actions = data |> of_data |> Log.flatten_imports(~of_data);
        let current =
          History.Model.reset(
            ~font_metrics=model.current.current.globals.font_metrics, // Keep old font metrics - otherwise it goes weird
            (),
          );
        // Retain log panel after import
        let current = {
          ...current,
          current: {
            ...current.current,
            globals: {
              ...current.current.globals,
              settings: {
                ...current.current.globals.settings,
                show_log_panel: true,
                sidebar: model.current.current.globals.settings.sidebar,
              },
            },
          },
        };
        {
          ...model,
          current,
          future_log: actions,
          replay_messages: [
            "Imported log entries: " ++ string_of_int(List.length(actions)),
            ...model.replay_messages,
          ],
        }
        |> Updated.return_quiet;
      | NextLog =>
        switch (model.future_log) {
        | [] =>
          {
            ...model,
            replay_messages: [
              "log replay finished",
              ...model.replay_messages,
            ],
            replay_toggle: false,
          }
          |> Updated.return_quiet
        | [(t, next), ...rest] =>
          print_endline("Applying next log action...");
          try({
            let updated =
              History.Update.update(
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
                future_log: rest,
                past_log: [(t, next), ...model.past_log],
                replay_messages: model.replay_messages,
                replay_toggle: model.replay_toggle,
              },
            };
          }) {
          | _ =>
            LogSidebar.log_error("Failed to apply log action");
            Model.{
              ...model,
              replay_messages: [
                "Error applying log action : " ++ History.Update.show(next),
                ...model.replay_messages,
              ],
              future_log: model.future_log,
              replay_toggle: false,
            }
            |> return_quiet;
          };
        }
      | SkipLog =>
        switch (model.future_log) {
        | [] =>
          {
            ...model,
            replay_messages: [
              "No log entry to skip",
              ...model.replay_messages,
            ],
          }
          |> return_quiet
        | [(_, _), ...rest] =>
          {
            ...model,
            replay_messages: [
              "Skipped a log entry",
              ...model.replay_messages,
            ],
            future_log: rest,
          }
          |> return_quiet
        }
      | ToggleReplay =>
        Model.{
          ...model,
          replay_toggle: !model.replay_toggle,
        }
        |> return_quiet
      | ClearLog =>
        Log.DB.clear_and(() => ());
        {
          ...model,
          future_log: [],
          past_log: [],
          replay_messages: [
            "Cleared all log entries",
            ...model.replay_messages,
          ],
        }
        |> return_quiet;
      }
    | action =>
      let current =
        History.Update.update(
          ~import_log,
          ~get_log_and,
          ~schedule_action,
          action,
          model.current,
        );
      let _ = Log.update(action, current);
      {
        ...current,
        model: {
          current: current.model,
          future_log: model.future_log,
          past_log: model.past_log,
          replay_toggle: model.replay_toggle,
          replay_messages: model.replay_messages,
        },
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
      |> History.Update.calculate(~schedule_action, ~is_edited, ~dynamics),
    future_log: model.future_log,
    past_log: model.past_log,
    replay_messages: model.replay_messages,
    replay_toggle: model.replay_toggle,
  };
};

module Selection = {
  type t = History.Selection.t;

  let handle_key_event = (model: Model.t) =>
    History.Selection.handle_key_event(model.current);

  let get_cursor_info = (model: Model.t) =>
    History.Selection.get_cursor_info(model.current);
};

module View = {
  let view =
      (~get_log_and, ~inject: Update.t => Ui_effect.t(unit), model: Model.t) => {
    History.View.view(
      ~log_model=
        LogSidebar.Model.{
          messages: model.replay_messages,
          is_playing: model.replay_toggle,
          current_step: List.length(model.past_log),
          total_steps:
            List.length(model.past_log) + List.length(model.future_log),
          show_details: true,
        },
      ~get_log_and,
      ~inject,
      model.current,
    );
  };
};
