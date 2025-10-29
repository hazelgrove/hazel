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
    | Globals(HistoryJump(id)) =>
      print_endline("Searching through undo stack");
      let current: list(Updated.t(EditHistory.state)) =
        if (Page.Update.can_undo(model.current.action)) {
          [model.current |> return]; // TODO Should this always be a |> return?
        } else {
          [];
        };
      // Look for history jump in the undo stack
      let (xs, ys) =
        ListUtil.take_while(
          (update: Updated.t(EditHistory.state)) => update.model.id != id,
          model.undo_stack,
        );
      switch (ys) {
      | [] =>
        print_endline("Searching through redo stack");
        // The history jump is not in the undo stack, so look in the redo stack
        let (xs: list(Updated.t(EditHistory.state)), ys) =
          ListUtil.take_while(
            (update: Updated.t(EditHistory.state)) => update.model.id != id,
            model.redo_stack,
          );
        switch (ys) {
        | [] => model |> return_quiet
        | [y, ...ys] =>
          let x: Model.t = {
            current: y.model,
            undo_stack: List.rev(xs) @ current @ model.undo_stack,
            redo_stack: ys,
          };
          x |> return;
        };
      | [y, ...ys] =>
        let x: Model.t = {
          current: y.model,
          undo_stack: ys,
          redo_stack: List.rev(xs) @ current @ model.redo_stack,
        };
        x |> return;
      };
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
