open Virtual_dom.Vdom;
open Node;

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
};
