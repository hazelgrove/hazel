open Haz3lcore;
include UpdateAction;

//TODO(andrew): this is duplicated from Update
let perform_action = (model: Model.t, a: Action.t): Result.t(Model.t) => {
  let ed_init = Editors.get_editor(model.editors);
  switch (Haz3lcore.Perform.go(~settings=model.settings.core, a, ed_init)) {
  | Error(err) => Error(FailedToPerform(err))
  | Ok(ed) => Ok({...model, editors: Editors.put_editor(ed, model.editors)})
  };
};

let schedule_prompt =
    (~settings, ~ctx_init: Ctx.t, zipper: Zipper.t, ~schedule_action): unit =>
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
    schedule_action(Assistant(Prompt(Oracle)));
  | (Outer, _, [Tile({label: [s], _}), ..._])
      when Str.string_match(Str.regexp("^\\?$"), s, 0) =>
    schedule_action(PerformAction(Select(Term(Current))));
    schedule_action(
      Assistant(
        Prompt(
          Filler(
            Some({
              llm: Azure_GPT4, //Azure_GPT3_5Turbo,
              prompt_builder: Filler.prompt(~settings, ~ctx_init),
            }),
          ),
        ),
      ),
    );
  | _ => ()
  };

let reset_buffer = (model: Model.t) => {
  let ed = model.editors |> Editors.get_editor;
  let z = ed.state.zipper;
  switch (z.selection.mode) {
  | Buffer(_) =>
    switch (Perform.go_z(~settings=model.settings.core, Destruct(Left), z)) {
    | Error(_) => model
    | Ok(z) =>
      let ed = Editor.new_state(Destruct(Left), z, ed);
      //TODO(andrew): fix double action
      {...model, editors: Editors.put_editor(ed, model.editors)};
    }
  | _ => model
  };
};

//TODO(andrew): add special case for when it just returns the whole sketch instead of completion

let apply =
    (
      {settings, _} as model: Model.t,
      update: agent_action,
      ~schedule_action,
      ~state,
      ~main,
    )
    : Result.t(Model.t) => {
  let editor = model.editors |> Editors.get_editor;
  let z = editor.state.zipper;
  switch (update) {
  | Prompt(Weather) =>
    WeatherAPI.request(req =>
      switch (WeatherAPI.handle(req)) {
      | Some(str) => schedule_action(Paste(Form.string_quote(str)))
      | None => print_endline("WeatherAPI: response parse failed")
      }
    );
    Ok(model);
  | Prompt(Oracle) =>
    switch (Oracle.ask(model)) {
    | None => print_endline("Oracle: prompt generation failed")
    | Some(prompt) =>
      OpenAI.start_chat(~llm=Azure_GPT4, prompt, req =>
        switch (OpenAI.handle_chat(req)) {
        | Some(response) => schedule_action(Oracle.react(response))
        | None => print_endline("Assistant: response parse failed")
        }
      )
    };
    Ok(model);
  | Prompt(Filler(None)) =>
    print_endline("deprecated");
    Ok(model);
  | Prompt(Filler(Some({llm, prompt_builder}))) =>
    switch (prompt_builder(editor)) {
    | None => print_endline("Filler: prompt generation failed")
    | Some(prompt) =>
      print_endline("GENERATED PROMPT:\n " ++ OpenAI.show_prompt(prompt));
      OpenAI.start_chat(~llm, prompt, req =>
        switch (OpenAI.handle_chat(req)) {
        | Some(response) =>
          let trim_indentation = s =>
            Js_of_ocaml.Regexp.global_replace(
              Js_of_ocaml.Regexp.regexp("\n(\\s)*"),
              s,
              "\n",
            );
          print_endline("Filler: calling react_error");
          let ctx_init = Editors.get_ctx_init(~settings, model.editors);
          switch (
            ChatLSP.Type.ctx(~settings, ~ctx_init, editor),
            ChatLSP.Type.mode(~settings, ~ctx_init, editor),
          ) {
          | (Some(init_ctx), Some(mode)) =>
            switch (Filler.error_reply(~settings, response, ~init_ctx, ~mode)) {
            | None =>
              print_endline("react_error: no errors.");
              print_endline("RECEIVED RESPONSE:\n " ++ response);
              print_endline(
                "TRIMMED RESPONSE:\n " ++ trim_indentation(response),
              );
              let trimmed_response = trim_indentation(response);
              schedule_action(Assistant(SetBuffer(trimmed_response)));
              schedule_action(SetMeta(Auto(EndTest())));
            | Some(reply) =>
              print_endline("react_error: errors:" ++ reply);
              OpenAI.reply_chat(~llm, prompt, response, reply, req =>
                switch (OpenAI.handle_chat(req)) {
                | Some(response) =>
                  print_endline("RECEIVED RESPONSE:\n " ++ response);
                  print_endline(
                    "TRIMMED RESPONSE:\n " ++ trim_indentation(response),
                  );
                  let trimmed_response = trim_indentation(response);
                  schedule_action(Assistant(SetBuffer(trimmed_response)));
                  schedule_action(SetMeta(Auto(EndTest())));
                | None => print_endline("Filler: handler failed")
                }
              );
            }
          | _ =>
            print_endline("react_error: no CI");
            schedule_action(Assistant(SetBuffer(response)));
            schedule_action(SetMeta(Auto(EndTest())));
          };
        | None => print_endline("Filler: handler failed")
        }
      );
    };
    Ok(model);
  | Prompt(TyDi) =>
    let ctx_init = Editors.get_ctx_init(~settings, model.editors);
    switch (TyDi.set_buffer(~settings=settings.core, ~ctx=ctx_init, z)) {
    | None => Ok(model)
    | Some(z) =>
      let ed = Editor.new_state(Pick_up, z, editor);
      //TODO: add correct action to history (Pick_up is wrong)
      let editors = Editors.put_editor(ed, model.editors);
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
      | None => Ok(model)
      /*TODO(andrew): shouldnt happen
        if we assume that we prevalidate everything we put
        in the amorphous buffer*/
      | Some(completion) =>
        //print_endline("accept suggestion: smart complete");
        main(model, Paste(completion), state, ~schedule_action)
      }
    }
  | SetBuffer(response) =>
    // print_endline("paste into selection: " ++ str);
    switch (Printer.paste_into_zip(Zipper.init(), response)) {
    | None => Error(CantSuggest)
    | Some(response_z) =>
      let content = Zipper.unselect_and_zip(response_z);
      let z = Zipper.set_buffer(editor.state.zipper, ~content, ~mode=Solid);
      //print_endline("paste into selection suceeds: " ++ Zipper.show(z));
      //HACK(andrew): below is not strictly a insert action...
      let ed = Editor.new_state(Insert(response), z, editor);
      let editors = Editors.put_editor(ed, model.editors);
      Ok({...model, editors});
    }
  };
};
