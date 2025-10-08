open Util;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type state = Page.Model.t;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: EditHistory.state,
    undo_stack: list(Updated.t(EditHistory.state)),
    redo_stack: list(Updated.t(EditHistory.state)),
  };

  let equal = (===);

  let init = () => {
    current: {
      id: Id.mk(),
      action: EditHistory.Update.Start,
      page: Page.Store.load(),
    },
    undo_stack: [],
    redo_stack: [],
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
          },
        }
      }
    | Globals(HistoryJump(id)) => model |> return_quiet
    /*let rest =
        ListUtil.drop_while(
          (update: Updated.t(EditHistory.state)) => update.model.id != id,
          model.history_log,
        );
      switch (rest) {
      | [] => model |> return
      | [x, ...xs] =>
        let x: Model.t = {
          current: x.model.page,
          undo_stack: [],
          redo_stack: [],
          history_log: [x, ...xs],
        };
        x |> return;
      };*/
    | action =>
      let current =
        Page.Update.update(
          ~import_log,
          ~get_log_and,
          ~schedule_action,
          action,
          model.current.page,
        );
      if (Page.Update.can_undo(action)) {
        {
          ...current,
          model: {
            current: {
              id: Id.mk(),
              action,
              page: current.model,
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
              page: current.model,
              action: model.current.action,
              id: model.current.id,
            },
            undo_stack: model.undo_stack,
            redo_stack: model.redo_stack,
          },
        };
      };
    };

  let calculate =
      (~schedule_action: t => unit, ~is_edited: bool, model: Model.t): Model.t => {
    current: {
      ...model.current,
      page:
        model.current.page
        |> Page.Update.calculate(~schedule_action, ~is_edited),
    },
    undo_stack: model.undo_stack,
    redo_stack: model.redo_stack,
  };
};

module Selection = {
  type t = Editors.Selection.t;

  let handle_key_event = (model: Model.t) =>
    Page.Selection.handle_key_event(model.current.page);

  let get_cursor_info = (model: Model.t) =>
    Page.Selection.get_cursor_info(model.current.page);
};

module View = {
  let view =
      (~get_log_and, ~inject: Update.t => Ui_effect.t(unit), model: Model.t) => {
    let extract_models =
        (stack: list(Updated.t(EditHistory.state)))
        : list(EditHistory.state) =>
      List.map(
        (updated: Updated.t(EditHistory.state)) => updated.model,
        stack,
      );

    Page.View.view(
      ~get_log_and,
      ~inject,
      model.current.page,
      (
        extract_models(model.redo_stack),
        model.current,
        extract_models(model.undo_stack),
      ),
    );
  };
};
