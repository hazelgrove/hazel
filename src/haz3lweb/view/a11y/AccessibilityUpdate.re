open Haz3lcore;

let remove_last_char = (str: string) =>
  if (String.length(str) > 0) {
    String.sub(str, 0, String.length(str) - 1);
  } else {
    str;
  };

let edit_input = (input: string, action: Action.t) => {
  switch (action) {
  | Action.Insert(s) => input ++ s
  | Action.Destruct(Left) => remove_last_char(input)
  | _ => input
  };
};

type update =
  | Edit(Action.t);

let update_model =
    (
      ~settings: Settings.t,
      ~ctx_init,
      ~editor: Editor.t,
      model: AccessibilityModel.t,
      update: update,
    ) =>
  switch (update) {
  | Edit(action) =>
    let input = edit_input(model.input, action);
    let parseResult = QueryEngine.query_parser(input);
    let model = {...model, input};
    switch (parseResult) {
    | None => model
    | Some(command) =>
      let r =
        QueryEngine.execute_query(~settings, ~ctx_init, ~editor, command);
      let query_result = Some(r.result);
      {...model, query_result};
    };
  };
