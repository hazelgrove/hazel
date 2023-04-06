open Haz3lcore;
include UpdateAction;

//TODO(andrew): this is duplicated from Update
let perform_action = (model: Model.t, a: Action.t): Result.t(Model.t) => {
  let (id, ed_init) = Editors.get_editor_and_id(model.editors);
  switch (Haz3lcore.Perform.go(a, ed_init, id)) {
  | Error(err) => Error(FailedToPerform(err))
  | Ok((ed, id)) =>
    Ok({...model, editors: Editors.put_editor_and_id(id, ed, model.editors)})
  };
};

let schedule_prompt = (zipper: Zipper.t, ~schedule_action): unit =>
  switch (
    zipper.caret,
    zipper.relatives.siblings |> snd,
    zipper.relatives.siblings |> fst |> List.rev,
  ) {
  | (Inner(_, c), [Tile({label: [s], _}), ..._], _)
      when
        Str.string_match(Str.regexp("^\".*\\?\"$"), s, 0)
        && c == String.length(s)
        - 2 =>
    schedule_action(PerformAction(Select(Term(Current))));
    schedule_action(Agent(Prompt(Oracle)));
  | (Outer, _, [Tile({label: [s], _}), ..._])
      when Str.string_match(Str.regexp("^\\?$"), s, 0) =>
    schedule_action(PerformAction(Select(Term(Current))));
    schedule_action(Agent(Prompt(Filler)));
  | _ => ()
  };

let reset_buffer = (model: Model.t) => {
  let (id, ed) = model.editors |> Editors.get_editor_and_id;
  let z = ed.state.zipper;
  switch (z.selection.mode) {
  | Buffer(_) =>
    switch (Perform.go_z(Destruct(Left), z, id)) {
    | Error(_) => model
    | Ok((z, id)) =>
      let ed = Editor.new_state(Destruct(Left), z, ed);
      //TODO(andrew): fix double action
      {...model, editors: Editors.put_editor_and_id(id, ed, model.editors)};
    }
  | _ => model
  };
};

let apply =
    (model: Model.t, update: agent_action, ~schedule_action, ~state, ~main)
    : Result.t(Model.t) => {
  let (id, ed) = model.editors |> Editors.get_editor_and_id;
  let z = ed.state.zipper;
  switch (update) {
  | Prompt(Weather) =>
    WeatherAPI.request(req =>
      switch (WeatherAPI.handle(req)) {
      | Some(str) => schedule_action(Paste("\"" ++ str ++ "\""))
      | None => print_endline("WeatherAPI: response parse failed")
      }
    );
    Ok(model);
  | Prompt(Oracle) =>
    switch (Oracle.ask(model)) {
    | None => print_endline("Oracle: prompt generation failed")
    | Some(prompt) =>
      OpenAI.request_chat(prompt, req =>
        switch (OpenAI.handle_chat(req)) {
        | Some(response) => schedule_action(Oracle.react(response))
        | None => print_endline("Assistant: response parse failed")
        }
      )
    };
    Ok(model);
  | Prompt(Filler) =>
    switch (Filler.prompt(model)) {
    | None => print_endline("Filler: prompt generation failed")
    | Some(prompt) =>
      OpenAI.request_chat(prompt, req =>
        switch (OpenAI.handle_chat(req)) {
        | Some(response) => schedule_action(Filler.react(response))
        | None => print_endline("Filler: handler failed")
        }
      )
    };
    Ok(model);
  | Prompt(TyDi) =>
    let ctx = Editors.get_ctx_init(model.editors);
    switch (TyDi.set_buffer(~ctx, z, id)) {
    | None => Ok(model)
    | Some((z, id)) =>
      let ed = Editor.new_state(Pick_up, z, ed);
      //TODO: add correct action to history (Pick_up is wrong)
      let editors = Editors.put_editor_and_id(id, ed, model.editors);
      Ok({...model, editors});
    };
  | AcceptSuggestion =>
    switch (z.selection.mode) {
    | Normal => Ok(model)
    | Buffer(Solid) =>
      //print_endline("accept suggestion: basic complete");
      perform_action(model, Unselect(Some(Right)))
    | Buffer(Amorphous) =>
      switch (TyDi.get_amorphous_buffer_text(z)) {
      | None => Ok(model) /*TODO(andrew): shouldnt happen
      if we assume that we prevalidate everything we put
      in the amorphous buffer*/
      | Some(completion) =>
        //print_endline("accept suggestion: smart complete");
        main(model, Paste(completion), state, ~schedule_action)
      }
    }
  | SetBuffer(response) =>
    // print_endline("paste into selection: " ++ str);
    switch (Printer.paste_into_zip(Zipper.init(id), id, response)) {
    | None => Error(CantSuggest)
    | Some((response_z, id)) =>
      let content = Zipper.unselect_and_zip(response_z);
      let z = Zipper.set_buffer(ed.state.zipper, ~content, ~mode=Solid);
      //print_endline("paste into selection suceeds: " ++ Zipper.show(z));
      //HACK(andrew): below is not strictly a insert action...
      let ed = Editor.new_state(Insert(response), z, ed);
      let editors = Editors.put_editor_and_id(id, ed, model.editors);
      Ok({...model, editors});
    }
  };
};
