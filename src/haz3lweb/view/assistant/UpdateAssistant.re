open Haz3lcore;
include UpdateAction;

let trim_indents = s =>
  Js_of_ocaml.Regexp.global_replace(
    Js_of_ocaml.Regexp.regexp("\n(\\s)*"),
    s,
    "\n",
  );

//TODO(andrew): this is duplicated from Update
let perform_action = (model: Model.t, a: Action.t): Result.t(Model.t) => {
  let ed_init = Editors.get_editor(model.editors);
  switch (Haz3lcore.Perform.go(~settings=model.settings.core, a, ed_init)) {
  | Error(err) => Error(FailedToPerform(err))
  | Ok(ed) => Ok({...model, editors: Editors.put_editor(ed, model.editors)})
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
    schedule_action(Assistant(Prompt(Oracle)));
  | (Outer, _, [Tile({label: [s], _}), ..._])
      when Str.string_match(Str.regexp("^\\?$"), s, 0) =>
    schedule_action(PerformAction(Select(Term(Current))));
    schedule_action(Assistant(Prompt(Filler(FillerOptions.init))));
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
      OpenAI.start_chat(~llm=Llama2, prompt, req =>
        switch (OpenAI.handle_chat(req)) {
        | Some(response) => schedule_action(Oracle.react(response))
        | None => print_endline("Assistant: response parse failed")
        }
      )
    };
    Ok(model);
  | Prompt(Filler({llm, _} as filter_otions)) =>
    let schedule_if_auto = action =>
      switch (model.meta.auto.current_script) {
      | None => ()
      | Some(name) => schedule_action(SetMeta(Auto(action(name))))
      };
    let ctx_init = Editors.get_ctx_init(~settings, model.editors);
    let add_round = add => schedule_if_auto(name => UpdateResult(name, add));
    switch (Filler.prompt(filter_otions, ~settings, ~ctx_init, editor)) {
    | None => print_endline("Filler: prompt generation failed")
    | Some(prompt) =>
      print_endline("GENERATED PROMPT:\n " ++ OpenAI.show_prompt(prompt));
      OpenAI.start_chat(~llm, prompt, req =>
        switch (OpenAI.handle_chat(req)) {
        | Some(response) =>
          switch (
            ChatLSP.Type.ctx(~settings, ~ctx_init, editor),
            ChatLSP.Type.mode(~settings, ~ctx_init, editor),
          ) {
          | (Some(init_ctx), Some(mode)) =>
            add_round(AddRoundOne(settings, init_ctx, mode, response));
            switch (Filler.error_reply(~settings, response, ~init_ctx, ~mode)) {
            | None =>
              print_endline("first round response:\n " ++ response);
              schedule_action(
                Assistant(SetBuffer(trim_indents(response))),
              );
              schedule_if_auto(_ => EndTest());
            | Some(reply) =>
              print_endline("first round: errors detected:" ++ reply);
              OpenAI.reply_chat(
                ~llm, prompt, ~assistant=response, ~user=reply, req =>
                switch (OpenAI.handle_chat(req)) {
                | Some(response) =>
                  add_round(AddRoundTwo(settings, init_ctx, mode, response));
                  print_endline("second round response:\n " ++ response);
                  schedule_action(
                    Assistant(SetBuffer(trim_indents(response))),
                  );
                  schedule_if_auto(_ => EndTest());
                | None => print_endline("Filler: handler failed")
                }
              );
            };
          | _ =>
            print_endline(
              "ERROR: UpdateAssistant: Filler: prompt generation failed",
            );
            ();
          }
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
    | Buffer(Solid) => perform_action(model, Unselect(Some(Right)))
    | Buffer(Amorphous) =>
      switch (TyDi.get_amorphous_buffer_text(z)) {
      | None => Ok(model)
      /* This case shouldn't happen if we assume that we prevalidate
       * everything we put in the amorphous buffer*/
      | Some(completion) when String.contains(completion, ' ') =>
        /* Slightly hacky. We assume that if a completion string has
         * spaces in it, that means it will have a hole in it. This
         * is a non-essential invariant currently maintained in TyDi.
         * In such a case, we insert the completion as normal by
         * pasting, then return to the beginning and advance to the
         * first hole. This should be revisited if completions are
         * refactored to use a more structured buffer format */
        module M = (val Editor.Meta.module_of_t(editor.state.meta));
        let start = Zipper.caret_point(M.measured, z);
        let rec do_actions = (model, actions: list(UpdateAction.t)) =>
          switch (actions) {
          | [] => Ok(model)
          | [hd, ...tl] =>
            switch (main(model, hd, state, ~schedule_action)) {
            | Error(err) => Error(err)
            | Ok(model) => do_actions(model, tl)
            }
          };
        /* TODO(andrew): use zipper-level actions here to avoid
         * measured recomputation at editor-level */
        do_actions(
          model,
          [
            Paste(completion),
            PerformAction(Move(Goal(Point(start)))),
            PerformAction(MoveToNextHole(Right)),
            PerformAction(Move(Local(Left(ByToken)))),
          ],
        );
      | Some(completion) =>
        main(model, Paste(completion), state, ~schedule_action)
      }
    }
  | SetBuffer(response) =>
    print_endline("SetBuffer: response: " ++ response);
    // print_endline("paste into selection: " ++ str);
    switch (Printer.paste_into_zip(Zipper.init(), response)) {
    | None =>
      print_endline("SetBuffer: A");
      Error(CantSuggest);
    | Some(response_z) =>
      print_endline("SetBuffer: B");
      let content = Zipper.unselect_and_zip(response_z);
      let z = Zipper.set_buffer(editor.state.zipper, ~content, ~mode=Solid);
      //print_endline("paste into selection suceeds: " ++ Zipper.show(z));
      //HACK(andrew): below is not strictly a insert action...
      let ed = Editor.new_state(Insert(response), z, editor);
      let editors = Editors.put_editor(ed, model.editors);
      print_endline("SetBuffer: C");
      Ok({...model, editors});
    };
  };
};
