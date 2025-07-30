open Util;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type current_state = Page.Model.t;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type history_state = {
    action: Page.Update.t,
    page: Page.Model.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: current_state,
    undo_stack: list(Updated.t(history_state)),
    redo_stack: list(Updated.t(history_state)),
  };

  let equal = (===);

  let init = () => {
    current: Page.Store.load(),
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
            current: x.model.page,
            undo_stack: rest,
            redo_stack: [
              {
                ...x,
                model: {
                  action,
                  page: model.current,
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
            current: x.model.page,
            undo_stack: [
              {
                ...x,
                model: {
                  action,
                  page: model.current,
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
          model.current,
        );
      //let history =
       // List.map(
        //  (s: Updated.t(Model.history_state)) => s.model.action,
       //   model.undo_stack,
      //  );
      //print_endline("---------------- UPDATE CALL ---------------");
      // print_endline("---HISTORY---");
      //List.iter(
      //  item => sexp_of_t(item) |> Sexplib.Sexp.to_string |> print_endline,
      //  history,
      //);
      //print_endline("---CURRENT ACTION---");
      //print_endline(Page.Update.sexp_of_t(action) |> Sexplib.Sexp.to_string);
      if (Page.Update.can_undo(action)) {
        print_endline("Undoable action");
        {
          ...current,
          model: {
            current: current.model,
            undo_stack: [
              {
                ...current,
                model: {
                  action,
                  page: model.current,
                },
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
    let history =
      //[model.current.action]
      List.map(
        (s: Updated.t(Model.history_state)) => s.model.action,
        model.undo_stack,
      );
    Page.View.view(~get_log_and, ~inject, model.current, history);
  };
};
