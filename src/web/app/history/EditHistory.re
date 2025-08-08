open Virtual_dom.Vdom;
open Node;
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
  action: Update.t,
  page: Model.t,
};

module View = {
  let history_view = history_log => {
    let grouped: list(list(Update.t)) =
      List.fold_left(
        // Lists are in reverse order during accumulation
        (acc: list(list(Update.t)), s: Updated.t(state)) =>
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
    let action_string = (item: Update.t) => {
      switch (item) {
      | Editors(Scratch(CellAction(MainEditor(Perform(action))))) =>
        Haz3lcore.Action.sexp_of_t(action) |> Sexplib.Sexp.to_string
      | _ => Update.sexp_of_t(item) |> Sexplib.Sexp.to_string
      };
    };
    let group_view = (group: list(Update.t)) => {
      switch (group) {
      | [] => div([]) // Shouldn't happen
      | [
          Editors(Scratch(CellAction(MainEditor(Perform(Insert(_)))))),
          ...rest,
        ] =>
        let str =
          List.fold_left(
            (acc, action: Update.t) =>
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
                (item: Update.t) => div([text(action_string(item))]),
                group,
              ),
          ),
        grouped',
      ),
    );
  };
};
