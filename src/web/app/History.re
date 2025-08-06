open Util;
open Virtual_dom.Vdom;
open Node;
open Haz3lcore;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type state = Page.Model.t;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type edit_history_state = {
    action: Page.Update.t,
    page: Page.Model.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: state,
    undo_stack: list(Updated.t(state)),
    redo_stack: list(Updated.t(state)),
    history_log: list(Updated.t(edit_history_state)),
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
  type t = Page.selection;

  let handle_key_event = (model: Model.t) =>
    Page.Selection.handle_key_event(model.current);

  let get_cursor_info = (model: Model.t) =>
    Page.Selection.get_cursor_info(model.current);
};

module View = {
  let history_view = history_log => {
    let grouped: list(list(Page.Update.t)) =
      List.fold_left(
        // Lists are in reverse order during accumulation
        (
          acc: list(list(Page.Update.t)),
          s: Updated.t(Model.edit_history_state),
        ) =>
          switch (acc) {
          | [] => [[s.model.action]]
          | [current_group, ...rest_group] =>
            switch (current_group) {
            | [] => [[]] // This shouldn't be able to happen
            | [entry, ...rest] =>
              switch (entry, s.model.action) {
              | (
                  Editors(
                    Scratch(CellAction(MainEditor(Perform(Insert(_))))),
                  ),
                  Editors(
                    Scratch(CellAction(MainEditor(Perform(Insert(_))))),
                  ),
                )
              | (
                  Editors(
                    Scratch(CellAction(MainEditor(Perform(Destruct(_))))),
                  ),
                  Editors(
                    Scratch(CellAction(MainEditor(Perform(Destruct(_))))),
                  ),
                ) => [
                  [s.model.action, ...current_group],
                  ...rest_group,
                ]
              | _ => [[s.model.action], ...acc]
              }
            }
          },
        [],
        history_log,
      );
    let grouped' = List.rev_map(List.rev, grouped);
    let action_string = (item: Page.Update.t) => {
      switch (item) {
      | Editors(Scratch(CellAction(MainEditor(Perform(action))))) =>
        Action.sexp_of_t(action) |> Sexplib.Sexp.to_string
      | _ => Page.Update.sexp_of_t(item) |> Sexplib.Sexp.to_string
      };
    };
    let group_view = (group: list(Page.Update.t)) => {
      switch (group) {
      | [] => div([]) // Shouldn't happen
      | [
          Editors(Scratch(CellAction(MainEditor(Perform(Insert(_)))))),
          ...rest,
        ] =>
        let str =
          List.fold_left(
            (acc, action: Page.Update.t) =>
              switch (action) {
              | Editors(
                  Scratch(CellAction(MainEditor(Perform(Insert(s))))),
                ) =>
                s ++ acc
              | _ => acc // Shouldn't happen
              },
            "",
            group,
          );
        div([text(str)]);
      | [first, ...rest] => div([text(action_string(first))])
      };
    };
    div(
      ~attrs=[Attr.id("edit-history")],
      List.mapi(
        (i, group) =>
          div(
            [text("Group " ++ string_of_int(i)), group_view(group)]
            @ List.map(
                (item: Page.Update.t) => div([text(action_string(item))]),
                group,
              ),
          ),
        grouped',
      ),
    );
  };

  let view =
      (~get_log_and, ~inject: Update.t => Ui_effect.t(unit), model: Model.t) => {
    Page.View.view(
      ~get_log_and,
      ~inject,
      model.current,
      history_view(model.history_log),
    );
  };
};
