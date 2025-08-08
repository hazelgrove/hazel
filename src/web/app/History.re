open Util;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type state = Page.Model.t;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: state,
    undo_stack: list(Updated.t(state)),
    redo_stack: list(Updated.t(state)),
    history_log: list(Updated.t(EditHistory.state)),
  };

  let equal = (===);

  let init = () => {
    current: Page.Store.load(),
    undo_stack: [],
    redo_stack: [],
    history_log: [],
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = EditHistory.Update.t;

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
            undo_stack: rest,
            redo_stack: [
              {
                ...x,
                model: model.current,
              },
              ...model.redo_stack,
            ],
            history_log: model.history_log,
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
            undo_stack: [
              {
                ...x,
                model: model.current,
              },
              ...model.undo_stack,
            ],
            redo_stack: rest,
            history_log: model.history_log,
          },
        }
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
      if (Page.Update.can_undo(action)) {
        {
          ...current,
          model: {
            current: current.model,
            undo_stack: [
              {
                ...current,
                model: model.current,
              },
              ...model.undo_stack,
            ],
            redo_stack: [],
            history_log: [
              {
                ...current,
                model: {
                  action,
                  page: model.current,
                },
              },
              ...model.history_log,
            ],
          },
        };
      } else {
        {
          ...current,
          model: {
            current: current.model,
            undo_stack: model.undo_stack,
            redo_stack: model.redo_stack,
            history_log: model.history_log,
          },
        };
      };
    };

  let calculate =
      (~schedule_action: t => unit, ~is_edited: bool, model: Model.t): Model.t => {
    current:
      model.current |> Page.Update.calculate(~schedule_action, ~is_edited),
    undo_stack: model.undo_stack,
    redo_stack: model.redo_stack,
    history_log: model.history_log,
  };
};

module Selection = {
  type t = Editors.Selection.t;

  let handle_key_event = (model: Model.t) =>
    Page.Selection.handle_key_event(model.current);

  let get_cursor_info = (model: Model.t) =>
    Page.Selection.get_cursor_info(model.current);
};

module View = {
  let view =
      (~get_log_and, ~inject: Update.t => Ui_effect.t(unit), model: Model.t) => {
    Page.View.view(~get_log_and, ~inject, model.current, model.history_log);
  };
};
