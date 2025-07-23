open Util;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type state = {
    action: Page.Update.t,
    model: Page.Model.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: state,
    undo_stack: list(Updated.t(state)),
    redo_stack: list(Updated.t(state)),
  };

  let equal = (===);

  let init = () => {
    current: {
      action: Page.Update.Start,
      model: Page.Store.load(),
    },
    undo_stack: [],
    redo_stack: [],
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
            undo_stack: rest,
            redo_stack: [
              {
                ...x,
                model: {
                  action,
                  model: model.current.model,
                },
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
            undo_stack: [
              {
                ...x,
                model: {
                  action,
                  model: model.current.model,
                },
              },
              ...model.undo_stack,
            ],
            redo_stack: rest,
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
          model.current.model,
        );
      print_endline(Page.Update.sexp_of_t(action) |> Sexplib.Sexp.to_string);
      if (Page.Update.can_undo(action)) {
        print_endline("Undoable action");
        {
          ...current,
          model: {
            current: {
              action,
              model: current.model,
            },
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
            current: {
              action,
              model: current.model,
            },
            undo_stack: model.undo_stack,
            redo_stack: model.redo_stack,
          },
        };
      };
    };

  let calculate =
      (
        ~schedule_action: t => unit,
        ~is_edited: bool,
        action: Page.Update.t,
        model: Model.t,
      )
      : Model.t => {
    current: {
      action, // TODO what action should actually be here if any?
      model:
        model.current.model
        |> Page.Update.calculate(~schedule_action, ~is_edited),
    },
    undo_stack: model.undo_stack,
    redo_stack: model.redo_stack,
  };
};

module Selection = {
  type t = Page.selection;

  let handle_key_event = (model: Model.t) =>
    Page.Selection.handle_key_event(model.current.model);

  let get_cursor_info = (model: Model.t) =>
    Page.Selection.get_cursor_info(model.current.model);
};

module View = {
  let view =
      (~get_log_and, ~inject: Update.t => Ui_effect.t(unit), model: Model.t) => {
    let history =
      //[model.current.action]
      List.map(
        (s: Updated.t(Model.state)) => s.model.action,
        model.undo_stack,
      );
    Page.View.view(~get_log_and, ~inject, model.current.model, history);
  };
};
