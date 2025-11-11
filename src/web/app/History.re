open Util;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type state = Page.Model.t;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: state,
    undo_stack: list(Updated.t(state)),
    redo_stack: list(Updated.t(state)),
    future_log: list(Page.Update.t),
  };

  let equal = (===);

  let init = () => {
    current: Page.Store.load(),
    undo_stack: [],
    redo_stack: [],
    future_log: [],
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Page.Update.t;

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
        model |> return_quiet;
      | [x, ...rest] => {
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
          },
        }
      }
    | Globals(Redo) =>
      switch (model.redo_stack) {
      | [] =>
        print_endline("Cannot redo");
        model |> return_quiet;
      | [x, ...rest] => {
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
          },
        }
      }
    | Globals(NextLog) =>
      switch (model.future_log) {
      | [] =>
        print_endline("No next log action to perform");
        model |> return_quiet;
      | [next, ...rest] =>
        print_endline(
          "Applying next log action: " ++ Page.Update.show(next),
        );
        let updated =
          try(
            Page.Update.update(
              ~import_log,
              ~get_log_and,
              ~schedule_action,
              next,
              model.current,
            )
          ) {
          | _ =>
            print_endline("Failed to apply log action");
            model.current |> Updated.return_quiet;
          };
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
          },
        };
      }
    | Globals(InitImportLog(f)) =>
      JsUtil.read_file(f, data =>
        schedule_action(Globals(FinishImportLog(data)))
      );
      model |> return_quiet;
    | Globals(FinishImportLog(None)) =>
      print_endline("Log import failed");
      model |> return_quiet;
    | Globals(FinishImportLog(Some(data))) =>
      let actions =
        data
        |> Export.import_just_log
        |> Sexplib.Sexp.of_string
        |> Log.Entry.s_of_sexp
        |> (
          x => {
            print_endline(
              "Imported log entries: " ++ string_of_int(List.length(x)),
            );
            x;
          }
        )
        |> List.map(((_ts, action)) => action);
      {
        ...model,
        future_log: model.future_log @ actions,
      }
      |> return_quiet;
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
