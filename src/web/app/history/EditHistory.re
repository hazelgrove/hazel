open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Globals(Globals.Update.t)
    | Editors(Editors.Update.t)
    | ExplainThis(ExplainThisUpdate.update)
    | Assistant(AssistantUpdate.t)
    | MakeActive(Editors.Selection.t)
    | Benchmark(Benchmark.action)
    | Start
    | Save;
};

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    globals: Globals.Model.t,
    editors: Editors.Model.t,
    explain_this: ExplainThisModel.t,
    assistant: AssistantModel.t,
    selection: Editors.Selection.t,
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type state = {
  id: Id.t,
  action: Update.t,
  page: Model.t,
};

module View = {
  let group_history = (history_log): list(list((Id.t, Update.t))) =>
    List.fold_left(
      // Lists are in reverse order during accumulation
      (acc: list(list((Id.t, Update.t))), s: state) =>
        switch (acc) {
        | [] => [[(s.id, s.action)]]
        | [current_group, ...rest_group] =>
          switch (current_group) {
          | [] => [[]] // This shouldn't be able to happen
          | [(id, entry), ...rest] =>
            switch (entry, s.action) {
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
                [(s.id, s.action), ...current_group],
                ...rest_group,
              ]
            | _ => [[(s.id, s.action)], ...acc]
            }
          }
        },
      [],
      history_log,
    );

  let draw_group = (~inject: Globals.Update.t => Ui_effect.t(unit), grouped) => {
    let action_string = (item: Update.t) => {
      switch (item) {
      | Editors(Scratch(CellAction(MainEditor(Perform(action))))) =>
        Haz3lcore.Action.sexp_of_t(action) |> Sexplib.Sexp.to_string
      | _ => Update.sexp_of_t(item) |> Sexplib.Sexp.to_string
      };
    };
    let group_view = (group: list((Id.t, Update.t))) => {
      switch (group) {
      | [] => div([]) // Shouldn't happen
      | [
          (
            _,
            Editors(Scratch(CellAction(MainEditor(Perform(Insert(_)))))),
          ),
          ...rest,
        ] =>
        let str =
          List.fold_left(
            (acc, (_, action: Update.t)) =>
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
      | [(_, first), ...rest] => div([text(action_string(first))])
      };
    };
    List.mapi(
      (i, group) =>
        div([
          text("Group " ++ string_of_int(i)),
          group_view(group),
          div(
            ~attrs=[clss(["collapse-group"])],
            List.map(
              ((id: Id.t, item: Update.t)) =>
                div(
                  ~attrs=[
                    Attr.on_click(_ => {
                      print_endline("Click!");
                      inject(HistoryJump(id));
                    }),
                  ],
                  [text(action_string(item))],
                ),
              group,
            ),
          ),
        ]),
      grouped,
    );
  };

  let history_view =
      (
        ~inject: Globals.Update.t => Ui_effect.t(unit),
        (redo_stack, current, undo_stack): (
          list(state),
          state,
          list(state),
        ),
      ) => {
    let undo_grouped: list(list((Id.t, Update.t))) =
      group_history([current, ...undo_stack]);

    let undo_grouped' = List.rev_map(List.rev, undo_grouped);

    div(
      ~attrs=[Attr.id("edit-history")],
      draw_group(~inject, undo_grouped'),
    );
  };
};
