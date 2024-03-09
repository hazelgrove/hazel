open Haz3lcore;
include UpdateAction;

let trim_indents = s =>
  Js_of_ocaml.Regexp.global_replace(
    Js_of_ocaml.Regexp.regexp("\n(\\s)*"),
    s,
    "\n",
  );

/* NOTE: this is duplicated from Update */
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
      let llm = OpenAI.Azure_GPT4;
      let key = OpenAI.lookup_key(llm);
      OpenAI.start_chat(~llm, ~key, prompt, req =>
        switch (OpenAI.handle_chat(req)) {
        | Some({content, _}) => schedule_action(Oracle.react(content))
        | None => print_endline("Assistant: response parse failed")
        }
      );
    };
    Ok(model);
  | Prompt(Filler({llm, _} as filler_options)) =>
    let key = OpenAI.lookup_key(llm);
    let schedule_if_auto = action =>
      switch (model.meta.auto.current_script) {
      | None => ()
      | Some(name) => schedule_action(SetMeta(Auto(action(name))))
      };
    let ctx_init = Editors.get_ctx_init(~settings, model.editors);
    let expected_ty = {
      let ctx_at_caret =
        switch (ChatLSP.Type.ctx(~settings, ~ctx_init, editor)) {
        | None => ctx_init
        | Some(ctx) => ctx
        };
      let mode = ChatLSP.Type.mode(~settings, ~ctx_init, editor);
      ChatLSP.Type.expected(~ctx=ctx_at_caret, mode);
    };
    let _add_round = add => schedule_if_auto(name => UpdateResult(name, add));
    switch (
      Filler.prompt(
        filler_options,
        ~sketch=Printer.to_string_editor(~holes=Some("?"), editor),
        ~expected_ty,
      ),
      ChatLSP.Type.ctx(~settings, ~ctx_init, editor),
      ChatLSP.Type.mode(~settings, ~ctx_init, editor),
    ) {
    | (Some(prompt), Some(init_ctx), Some(mode)) =>
      let rec error_loop =
              (~handler, ~fuel, prompt, reply: OpenAI.reply): unit => {
        //print_endline("Assistant: err rounds left: " ++ string_of_int(fuel));
        //print_endline("Assistant: reply.contents:" ++ reply.content);
        switch (Filler.error_reply(~init_ctx, ~mode, reply)) {
        | _ when fuel <= 0 =>
          //print_endline("Assistant: Error round limit reached, stopping");
          handler(reply.content)
        | None =>
          //print_endline("Assistant: No errors, stopping");
          handler(reply.content)
        | Some(err_msg) =>
          //print_endline("Assistant: reply errors:" ++ err_msg);
          let prompt' =
            OpenAI.add_to_prompt(
              prompt,
              ~assistant=reply.content,
              ~user=err_msg,
            );
          OpenAI.start_chat(~llm, ~key, prompt', response =>
            switch (OpenAI.handle_chat(response)) {
            | Some(reply') =>
              error_loop(~handler, ~fuel=fuel - 1, prompt', reply')
            | None => print_endline("WARN: Error loop: Handle returned none ")
            }
          );
        };
      };
      print_endline("Assistant: PROMPT:\n " ++ OpenAI.show_prompt(prompt));
      OpenAI.start_chat(~llm, ~key, prompt, req =>
        switch (OpenAI.handle_chat(req)) {
        | Some(reply) =>
          error_loop(
            ~fuel=filler_options.error_rounds_max,
            ~handler=
              reply =>
                schedule_action(Assistant(SetBuffer(trim_indents(reply)))),
            prompt,
            reply,
          )
        //add_round(AddRoundOne(settings, init_ctx, mode, reply));
        // switch (Filler.error_reply(~settings, ~init_ctx, ~mode, reply)) {
        // | None
        // | Some("") =>
        //   /* TODO(andrew): empty string kinda hacky here; refactor */
        //   print_endline("first round response:\n " ++ reply.content);
        //   schedule_action(
        //     Assistant(SetBuffer(trim_indents(reply.content))),
        //   );
        // //schedule_if_auto(_ => EndTest());
        // | Some(err_msg) =>
        //   print_endline("first round: errors detected:" ++ err_msg);
        //   let reply_prompt =
        //     add_to_prompt(prompt, ~assistant=reply.content, ~user=err_msg);
        //   OpenAI.start_chat(~llm, ~key, reply_prompt, req =>
        //     switch (OpenAI.handle_chat(req)) {
        //     | Some(reply2) =>
        //       //add_round(AddRoundTwo(settings, init_ctx, mode, reply2));
        //       //print_endline("second round response:\n " ++ reply2.content);
        //       schedule_action(
        //         Assistant(SetBuffer(trim_indents(reply2.content))),
        //       )
        //     //schedule_if_auto(_ => EndTest());
        //     | None => print_endline("Filler: handler failed")
        //     }
        //   );
        // }
        | None => print_endline("Filler: handler returned None")
        }
      );
    | _ => print_endline("Filler: prompt generation failed")
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
    | Buffer(Parsed) => perform_action(model, Unselect(Some(Right)))
    | Buffer(Unparsed) =>
      switch (TyDi.get_buffer(z)) {
      | None => Ok(model)
      /* This case shouldn't happen if we assume that we prevalidate
       * everything we put in the unparsed buffer*/
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
    switch (Printer.paste_into_zip(Zipper.init(), response)) {
    | None => Error(CantSuggest)
    | Some(response_z) =>
      let content = Zipper.unselect_and_zip(response_z);
      let z = Zipper.set_buffer(editor.state.zipper, ~content, ~mode=Parsed);
      //HACK(andrew): below is not strictly a insert action...
      let editor = Editor.new_state(Insert(response), z, editor);
      let editors = Editors.put_editor(editor, model.editors);
      Ok({...model, editors});
    }
  };
};
