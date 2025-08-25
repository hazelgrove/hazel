open Haz3lcore;
open Util;
open Util.OptUtil.Syntax;

open AssistantUpdateBase;
open AssistantUpdateComposition;

module CodeModel = CodeEditable.Model;
module Model = AssistantModel;

let can_undo = (action: t) => {
  // TODO: Implement the handling of actions that should be undoable
  // Thinking none of these actions should be undoable...
  // Maybe set API key?
  // That could be a good starter project to navigate this assistant part of the codebase.
  switch (action) {
  | SendMessage(_) => false
  | HandleResponse(_) => true /* Necessary to make completion instantiation undoable */
  | EmployLLMAction(_) => false
  | ChatAction(_) => false
  | InternalError(_) => false
  | ExternalAPIAction(_) => false
  | InitializeAssistant => false
  };
};

let update =
    (
      ~settings: Settings.t,
      ~action,
      ~model: Model.t,
      // todo: Find a way to track unqique editor between concurrent actions
      ~editor: CodeModel.t,
      ~schedule_action: t => unit,
      ~schedule_eval_action: AssistantEval.Update.t => unit,
      ~schedule_editor_action: Editors.Update.t => unit,
    )
    : Updated.t(Model.t) => {
  switch (action) {
  | SendMessage(kind, editor_opt, chat_id) =>
    let editor =
      switch (editor_opt) {
      | Some(editor) => editor
      | None => editor
      };
    if (model.current_chats.curr_tutor_chat == Id.invalid) {
      model |> Updated.return;
    } else {
      switch (kind) {
      | Tutor(content) =>
        let mode = AssistantSettings.HazelTutor;
        let curr_chat =
          Id.Map.find(chat_id, model.chat_history.past_tutor_chats);
        let content_message =
          mk_user_content_message(~content, ~role=User, ~editor);
        let ctx_message: Model.message = {
          content:
            Some(
              OpenRouter.mk_user_msg(
                String.concat(
                  "\n",
                  ChatLSP.get_sketch_and_error_ctx(editor),
                ),
              ),
            ),
          display:
            Some(
              Model.mk_message_display(
                ~content=
                  String.concat(
                    "\n",
                    ChatLSP.get_sketch_and_error_ctx(editor),
                  ),
              ),
            ),
          role: System(AssistantPrompt),
          sketch_snapshot: None,
        };

        let updated_chat =
          update_chat(curr_chat, [content_message, ctx_message]);

        mk_llm_call(
          ~mode,
          ~model,
          ~schedule_action,
          ~updated_chat,
          ~response_handler=response =>
          HandleResponse(Tutor, response, chat_id)
        );

        update_model_chat_history(
          ~model,
          ~mode,
          ~updated_chat,
          ~awaiting_response=true,
        )
        |> Updated.return;

      | Composition(kind, eval_mode) =>
        print_endline(
          "Here #6 : Composition Eval mode is set to "
          ++ string_of_bool(eval_mode),
        );
        let mode = AssistantSettings.TaskCompletion;
        let curr_chat =
          Id.Map.find(chat_id, model.chat_history.past_composition_chats);
        switch (kind) {
        | Intermediate =>
          intermediate_select_curr_node(~editor, ~schedule_editor_action);
          model |> Updated.return;

        // The initial message sent to the LLM via the User --
        // We can think of the agentic looping as a directed graph:
        // 1. The user sends a message to the LLM, appending with info from (2)
        // 2. We gather context of the program relative to the cursor
        //    and send it as a user message.
        // 3. The LLM responds with either a tool call or no tool call.
        //        if tool call, parse and handle it and then loop back to (2)
        //            (this cyclic edge is what makes enables the "agentic" nature)
        //        if no tool call, output final result to the user
        | Request(content) =>
          print_endline("Here #5 : Sending Composition Request");
          // This is step (1) of the directed graph example above.
          // The user sends a message to the LLM, appending with info from (2)
          // Note: (2) is done here, jointly with (1) and done in Loop(_, _) below,
          //       after a tool call has been handled.
          print_endline("handling composition request");
          let content_message: Model.message =
            mk_user_content_message(~content, ~role=User, ~editor);
          let (local_code_map_str, display) =
            AssistantModes.Composition.mk_local_code_map_prompt(
              ChatLSP.Options.init,
              editor,
            );
          let ctx_message: Model.message = {
            content: Some(local_code_map_str),
            display: Some(display),
            role: System(AssistantPrompt),
            sketch_snapshot: None,
          };

          let updated_chat =
            update_chat(curr_chat, [content_message, ctx_message]);

          mk_llm_call(
            ~mode,
            ~model,
            ~schedule_action,
            ~updated_chat,
            ~response_handler=response =>
            HandleResponse(
              CompositionLoopRound(
                editor,
                AssistantModes.Composition.max_tool_calls,
                eval_mode,
              ),
              response,
              chat_id,
            )
          );
          schedule_action(
            SendMessage(Composition(Intermediate, eval_mode), None, chat_id),
          );
          update_model_chat_history(
            ~model,
            ~mode,
            ~updated_chat,
            ~awaiting_response=true,
          )
          |> Updated.return;

        | Loop(fuel, tool_contents, status) =>
          // This is step (2) from the directed graph above --
          //    The agent just made a tool call. After
          //    (assumably) handling the tool call previously, we gather the new context
          //    from the program and cursor location, and then append these to our list
          //    of messages. This message is an OpenRouter tool message (as opposed to a user message),
          //    which takes the tool call and the tool call results (which we send as the context).
          //    We then send off this message to the LLM and await a response, either
          //    an end output to the user (implying no more looping) or a new tool call.
          schedule_action(
            SendMessage(Composition(Intermediate, eval_mode), None, chat_id),
          );
          let (local_code_map_str, display) =
            AssistantModes.Composition.mk_local_code_map_prompt(
              ChatLSP.Options.init,
              editor,
            );

          let local_code_map_str =
            "\n\nThe new AST context is:\n" ++ local_code_map_str.content;

          let updated_chat =
            switch (status) {
            | Success(response) =>
              let display = {
                ...display,
                displayable_content: [
                  Text(response),
                  ...display.displayable_content,
                ],
                raw_content: response ++ display.raw_content,
              };
              let response_message: Model.message = {
                content:
                  // TODO: fix this logic, because it is messy and redundant.
                  // We should maybe have mk_local_code_map_prompt always
                  // return an openrouter tool message, and deliberately inject
                  // an assistant tool call and tool response initially...
                  // or, if that is not feasible, then we should make the logic flow
                  // simpler to track overall
                  Some(
                    OpenRouter.mk_tool_msg(
                      response ++ local_code_map_str,
                      tool_contents,
                    ),
                  ),
                display: Some(display),
                role: System(AssistantPrompt),
                sketch_snapshot: None,
              };
              update_chat(curr_chat, [response_message]);
            | Failure(err) =>
              let err_message: Model.message = {
                content: Some(OpenRouter.mk_tool_msg(err, tool_contents)),
                display: Some(Model.mk_message_display(~content=err)),
                role: System(InternalError),
                sketch_snapshot: None,
              };
              update_chat(curr_chat, [err_message]);
            };

          mk_llm_call(
            ~mode,
            ~model,
            ~schedule_action,
            ~updated_chat,
            ~response_handler=response =>
            HandleResponse(
              CompositionLoopRound(editor, fuel, eval_mode),
              response,
              chat_id,
            )
          );

          update_model_chat_history(
            ~model,
            ~mode,
            ~updated_chat,
            ~awaiting_response=true,
          )
          |> Updated.return;
        };

      | Completion(kind) =>
        let mode = AssistantSettings.CodeSuggestion;
        switch (kind) {
        | Request(tile_id, advanced_reasoning) =>
          let new_chat = Model.new_chat(model, mode);
          print_endline("new_chat: " ++ Id.to_string(new_chat.id));
          let updated_past_chats =
            Model.add_chat_to_history(
              new_chat,
              model.chat_history.past_suggestion_chats,
            );
          let model_with_new_chat =
            resculpt_model(
              ~model,
              ~mode,
              ~updated_past_chats,
              ~chat_id=new_chat.id,
            );
          let tag = String.sub(Id.to_string(tile_id), 0, 3);
          switch (
            {
              let* sketch_z_with_tag =
                Perform.paste(editor.editor.state.zipper, tag);
              let sketch_seg =
                Zipper.smart_seg(
                  ~dump_backpack=true,
                  ~erase_buffer=true,
                  sketch_z_with_tag,
                );
              let* index = Indicated.index(editor.editor.state.zipper);
              let+ ci = Id.Map.find_opt(index, editor.statics.info_map);
              AssistantModes.Completion.mk_ctx_prompt(
                ChatLSP.Options.init,
                ci,
                sketch_seg,
                (advanced_reasoning ? "?a" : "??") ++ tag,
              );
            }
          ) {
          | None =>
            print_endline("Suggestion prompt generation failed");
            model_with_new_chat |> Updated.return;
          | Some(ctx_prompt) =>
            let ctx_message: Model.message = {
              content: Some(ctx_prompt),
              display:
                Some(Model.mk_message_display(~content=ctx_prompt.content)),
              role: System(AssistantPrompt),
              sketch_snapshot: None,
            };
            let updated_chat = update_chat(new_chat, [ctx_message]);
            mk_llm_call(
              ~mode,
              ~model,
              ~schedule_action,
              ~updated_chat,
              ~response_handler=response =>
              HandleResponse(
                CompletionErrorRound(
                  editor,
                  ChatLSP.Options.init.error_rounds_max,
                  tile_id,
                ),
                response,
                new_chat.id,
              )
            );
            update_model_chat_history(
              ~model=model_with_new_chat,
              ~mode=settings.assistant.mode,
              ~updated_chat,
              ~awaiting_response=true,
            )
            |> Updated.return;
          };
        | Query(content) =>
          let curr_chat =
            Id.Map.find(chat_id, model.chat_history.past_suggestion_chats);
          let ctx =
            OpenRouter.mk_user_msg(
              String.concat("\n", ChatLSP.get_sketch_and_error_ctx(editor)),
            );
          let ctx_message: Model.message = {
            content: Some(ctx),
            display: Some(Model.mk_message_display(~content=ctx.content)),
            role: System(AssistantPrompt),
            sketch_snapshot: None,
          };
          let content_message =
            mk_user_content_message(~content, ~role=User, ~editor);
          let updated_chat =
            update_chat(curr_chat, [ctx_message, content_message]);

          mk_llm_call(
            ~mode,
            ~model,
            ~schedule_action,
            ~updated_chat,
            ~response_handler=response =>
            HandleResponse(CompletionQueryResponse, response, chat_id)
          );

          update_model_chat_history(
            ~model,
            ~mode,
            ~updated_chat,
            ~awaiting_response=true,
          )
          |> Updated.return;

        | Loop(error, tile_id, fuel) =>
          let curr_chat =
            Id.Map.find(chat_id, model.chat_history.past_suggestion_chats);
          let error_message =
            OpenRouter.mk_user_msg(
              "Your previous response caused the following error. Please fix it in your response: "
              ++ error,
            );
          let error_message: Model.message = {
            content: Some(error_message),
            display:
              Some(Model.mk_message_display(~content=error_message.content)),
            role: System(AssistantPrompt),
            sketch_snapshot: None,
          };
          let updated_chat = update_chat(curr_chat, [error_message]);

          // check that fuel is not 0
          if (fuel < 0) {
            let content =
              "By default we stop the assistant after "
              ++ string_of_int(ChatLSP.Options.init.error_rounds_max)
              ++ " error rounds.";
            schedule_action(InternalError(content, mode, updated_chat.id));
          } else {
            mk_llm_call(
              ~mode,
              ~model,
              ~schedule_action,
              ~updated_chat,
              ~response_handler=response =>
              HandleResponse(
                CompletionErrorRound(editor, fuel, tile_id),
                response,
                chat_id,
              )
            );
          };
          update_model_chat_history(
            ~model,
            ~mode,
            ~updated_chat,
            ~awaiting_response=true,
          )
          |> Updated.return;
        };
      };
    };
  | InternalError(content, mode, chat_id) =>
    let curr_chat =
      switch (
        Id.Map.find_opt(chat_id, model.chat_history.past_composition_chats)
      ) {
      | Some(chat) => chat
      | None =>
        print_endline("Error: Chat not found");
        get_mode_info(mode, model) |> snd;
      };

    // todo: Should this be a user, assistant, or system message?
    //       We could make it assistant and put it in the first-person.
    let system_message: Model.message = {
      content: Some(OpenRouter.mk_user_msg(content)),
      display: Some(Model.mk_message_display(~content)),
      role: System(InternalError),
      sketch_snapshot: None,
    };

    // Note: We aren't sending a message here, but we do add it to the chat history.
    //       for future reference for the LLM so it isn't confused.
    //       (Eg: Max tool call limit reached, agent should know from history that this
    //        is why their prior task completion was not successful.)
    let updated_chat = {
      ...curr_chat,
      messages: curr_chat.messages @ [system_message],
    };
    update_model_chat_history(
      ~model,
      ~mode,
      ~updated_chat,
      ~awaiting_response=false,
    )
    |> Updated.return;

  | HandleResponse(response_kind, reply, chat_id) =>
    let (curr_chat, mode) =
      switch (response_kind) {
      | Tutor => (
          Id.Map.find(chat_id, model.chat_history.past_tutor_chats),
          AssistantSettings.HazelTutor,
        )
      | CompositionLoopRound(_) => (
          Id.Map.find(chat_id, model.chat_history.past_composition_chats),
          AssistantSettings.TaskCompletion,
        )
      | CompletionErrorRound(_) => (
          Id.Map.find(chat_id, model.chat_history.past_suggestion_chats),
          AssistantSettings.CodeSuggestion,
        )
      | CompletionQueryResponse => (
          Id.Map.find(chat_id, model.chat_history.past_suggestion_chats),
          AssistantSettings.CodeSuggestion,
        )
      };

    // todo: turning off for now to save credits
    //create_chat_descriptor(~model, ~schedule_action, ~mode, ~chat_id);
    let threshold =
      int_of_float(
        float_of_int(model.external_api_info.set_model_info.context_length)
        *. Model.context_threshold_ratio,
      );
    // Thin wrapper to avoid need of passing response.usage.total_tokens
    let summarize_chat = () =>
      if (reply.usage.total_tokens > threshold) {
        summarize_chat(model, curr_chat, mode, schedule_action);
      };

    let content = reply.content;
    print_endline("content: " ++ content);
    // Todo: Allow for multiple tool calls
    let tool_call = ListUtil.hd_opt(reply.tool_calls);
    let assistant_message: Model.message = {
      content:
        Some(OpenRouter.mk_assistant_msg(content, reply.tool_calls_json)),
      display:
        switch (content) {
        | "" => None
        | _ => Some(Model.mk_message_display(~content))
        },
      role: Assistant,
      sketch_snapshot: None,
    };

    // This commented out code below is for streaming, if we ever choose to add
    // If streaming, update the last message display
    let updated_chat = update_chat(curr_chat, [assistant_message]);
    /* let last_display = ListUtil.last(curr_chat.message_displays);
       if (last_display.role == Assistant) {
         let updated_content = last_display.original_content ++ content;
         (
           ListUtil.leading(curr_chat.messages)
           @ [OpenRouter.mk_assistant_msg(updated_content)],
           ListUtil.leading(curr_chat.messages)
           @ [
             Model.mk_message_display(
               ~content=updated_content,
               ~role=Assistant,
             ),
           ],
         );
       } else */

    switch (response_kind) {
    | Tutor =>
      summarize_chat();
      update_model_chat_history(
        ~model,
        ~mode,
        ~updated_chat,
        ~awaiting_response=false,
      )
      |> Updated.return;
    | CompositionLoopRound(_, fuel, eval_mode) =>
      print_endline(
        "Here #7 : Composition Eval mode is set to "
        ++ string_of_bool(eval_mode),
      );
      // This is step (3) from the directed graph above --
      switch (tool_call, fuel) {
      | (None, _) =>
        // The agent did not make a tool call, thus there is nothing to handle on the backend,
        // we can proceed as if there were a normal LLM chat interaction.
        summarize_chat();
        if (eval_mode) {
          schedule_eval_action(CollectResults);
        };
        update_model_chat_history(
          ~model,
          ~mode,
          ~updated_chat,
          ~awaiting_response=false,
        )
        |> Updated.return;
      | (_, 0) =>
        // The agent ran out of fuel. We should experiment with this in the future.
        if (eval_mode) {
          schedule_eval_action(CollectResults);
        };
        schedule_action(
          InternalError(
            "By default, we stop the agent after "
            ++ string_of_int(AssistantModes.Composition.max_tool_calls)
            ++ " tool calls.",
            mode,
            chat_id,
          ),
        );
        summarize_chat();
        update_model_chat_history(
          ~model,
          ~mode,
          ~updated_chat,
          ~awaiting_response=false,
        )
        |> Updated.return;
      | (Some(tool_call), _) =>
        let updated_chat = {
          let structure_edit_message: Model.message = {
            content: None,
            display:
              Some(
                Model.mk_message_display(
                  ~content=mk_structure_edit_msg(~tool_call),
                ),
              ),
            role: Tool,
            sketch_snapshot: None,
          };
          update_chat(
            curr_chat,
            [assistant_message, structure_edit_message],
          );
        };
        // We don't summarize while the agent loops on tool calls.

        // The agent made a tool call, we need to handle it and then perform a loop
        // round (the loop round itself will later handle it)
        let loop_message = (status: status) =>
          SendMessage(
            Composition(
              Loop(
                fuel - 1,
                {
                  tool_call_id: tool_call.id,
                  name: tool_call.tool_name,
                },
                status,
              ),
              eval_mode,
            ),
            None,
            chat_id,
          );
        let apply_action =
          AssistantModes.Composition.apply_action(
            ~schedule_action=schedule_editor_action,
            ~editor,
          );
        apply_structure_action(
          ~tool_call,
          ~apply_action,
          ~schedule_action,
          ~loop_message,
        );
        update_model_chat_history(
          ~model,
          ~mode,
          ~updated_chat,
          ~awaiting_response=false,
        )
        |> Updated.return;
      };
    | CompletionErrorRound(editor, fuel, tileId) =>
      // Split response into discussion and completion
      let code_pattern =
        Str.regexp(
          "\\(\\(.\\|\n\\)*\\)```[ \n]*\\([^`]+\\)[ \n]*```\\(\\(.\\|\n\\)*\\)",
        );
      let index = Option.get(Indicated.index(editor.editor.state.zipper));
      let ci = Option.get(Id.Map.find_opt(index, editor.statics.info_map));
      let sketch_z = editor.editor.state.zipper;

      let (_, completion) =
        if (Str.string_match(code_pattern, content, 0)) {
          let before = String.trim(Str.matched_group(1, content));
          let code = String.trim(Str.matched_group(3, content));
          (before, code |> StringUtil.trim_leading);
        } else {
          print_endline("Regex match failed for: " ++ content);
          ("", content |> StringUtil.trim_leading); // Fallback if no code block found
        };

      switch (
        AssistantModes.Completion.ErrorRound.mk_reply(
          ci,
          sketch_z,
          completion,
        )
      ) {
      | None =>
        print_endline("ERROR ROUNDS (Non-error Response): " ++ completion);
        schedule_action(
          EmployLLMAction(RemoveAndSuggest(completion, tileId)),
        );
      | Some(error) =>
        print_endline("ERROR ROUNDS (Error): " ++ error);
        print_endline(
          "ERROR ROUNDS (Error-causing Response): " ++ completion,
        );
        schedule_action(
          SendMessage(
            Completion(Loop(error, tileId, fuel - 1)),
            None,
            chat_id,
          ),
        );
      };
      update_model_chat_history(
        ~model,
        ~mode,
        ~updated_chat,
        ~awaiting_response=false,
      )
      |> Updated.return;
    | CompletionQueryResponse =>
      update_model_chat_history(
        ~model,
        ~mode,
        ~updated_chat,
        ~awaiting_response=false,
      )
      |> Updated.return
    };
  | EmployLLMAction(action) =>
    let add_suggestion =
      AssistantModes.Completion.add_suggestion(
        ~schedule_action=schedule_editor_action,
      );
    switch (action) {
    | Summarize(content, mode, chat_id) =>
      let (past_chats, _) = get_mode_info(mode, model);
      let curr_chat = Id.Map.find(chat_id, past_chats);
      // Only keep the prompt
      // Decide what else to keep here (last few code contexts?)
      let truncated_messages: list(Model.message) =
        List.filter_map(
          (message: Model.message) => {
            switch (message.content) {
            | Some(content) =>
              switch (content.role) {
              | Assistant =>
                Some({
                  ...message,
                  content: None,
                })
              | User =>
                Some({
                  ...message,
                  content: None,
                })
              | _ => Some(message)
              }
            | None => None
            }
          },
          curr_chat.messages,
        );
      let truncated_chat = {
        ...curr_chat,
        messages: truncated_messages,
      };
      let summarization_message_content = "Approaching Context Limit: A summary of the chat has been generated...";
      let summarization_message: Model.message = {
        content: Some(OpenRouter.mk_user_msg(summarization_message_content)),
        display:
          Some(
            Model.mk_message_display(~content=summarization_message_content),
          ),
        role: Tool,
        sketch_snapshot: None,
      };
      let summarized_chat_message: Model.message = {
        // note: making this an outgoing assistant message, but displaying as system message,
        // as it might make more sense for assistant to see that it or some other LLM made a summary,
        // and it might be more intuitive for user to see the summary as a collapsable system prompt,
        // (akin to init prompt/sketch contexts)
        content: Some(OpenRouter.mk_assistant_msg(content, [])),
        display: Some(Model.mk_message_display(~content)),
        role: System(AssistantPrompt),
        sketch_snapshot: None,
      };
      let updated_chat =
        update_chat(
          truncated_chat,
          [summarization_message, summarized_chat_message],
        );
      update_model_chat_history(
        ~model,
        ~mode,
        ~updated_chat,
        ~awaiting_response=false,
      )
      |> Updated.return;

    | RemoveAndSuggest(response, tileId) =>
      // Only side effects in the editor are performed here
      add_suggestion(~response, ~tile=tileId);
      model |> Updated.return;
    | Describe(content, mode, chat_id) =>
      let (past_chats, _) = get_mode_info(mode, model);
      let updated_past_chats =
        Id.Map.update(
          chat_id,
          opt_chat =>
            switch (opt_chat) {
            | Some(chat: Model.chat) =>
              Some({
                ...chat,
                descriptor: content,
              })
            | None => None
            },
          past_chats,
        );
      let curr_chat_id =
        switch (mode) {
        | HazelTutor => model.current_chats.curr_tutor_chat
        | CodeSuggestion => model.current_chats.curr_suggestion_chat
        | TaskCompletion => model.current_chats.curr_composition_chat
        };
      resculpt_model(
        ~model,
        ~mode,
        ~updated_past_chats,
        ~chat_id=curr_chat_id,
      )
      |> Updated.return;
    | SetLoop(loop) =>
      {
        ...model,
        loop,
      }
      |> Updated.return
    };

  | ChatAction(action) =>
    switch (action) {
    | NewChat =>
      print_endline("Here #2 : Adding Chat");
      let mode = settings.assistant.mode;
      let (past_chats, _) = get_mode_info(mode, model);
      let new_chat: Model.chat = Model.new_chat(model, mode);
      let updated_history = Model.add_chat_to_history(new_chat, past_chats);
      resculpt_model(
        ~model,
        ~mode,
        ~updated_past_chats=updated_history,
        ~chat_id=new_chat.id,
      )
      |> Updated.return;
    | DeleteChat(chat_to_be_gone_id) =>
      let mode = settings.assistant.mode;
      // Filter out the chat we're deleting
      let (past_chats, curr_chat) = get_mode_info(mode, model);
      let filtered_past_chats =
        Id.Map.filter((id, _) => id != chat_to_be_gone_id, past_chats);
      let chrono_history = Model.sorted_chats(filtered_past_chats);
      let updated_model =
        curr_chat.id == chat_to_be_gone_id
          ? switch (ListUtil.hd_opt(chrono_history)) {
            | Some(chat) =>
              resculpt_model(
                ~model,
                ~mode,
                ~updated_past_chats=filtered_past_chats,
                ~chat_id=chat.id,
              )
            | None =>
              resculpt_model(
                ~model,
                ~mode,
                ~updated_past_chats=past_chats,
                ~chat_id=curr_chat.id,
              )
            }
          : resculpt_model(
              ~model,
              ~mode,
              ~updated_past_chats=filtered_past_chats,
              ~chat_id=curr_chat.id,
            );
      updated_model |> Updated.return;

    // Concat LS' error message and await_llm_response (... animation)
    // This works even if out of fuel, as both Respond and ErrorRespond
    // remove await_llm_response
    | CollapseMessage(index) =>
      let mode = settings.assistant.mode;
      let (past_chats, curr_chat) = get_mode_info(mode, model);
      let is_prompt_display =
        try(
          List.nth(curr_chat.messages, index).role == System(AssistantPrompt)
        ) {
        | Invalid_argument(_) => true
        };
      let updated_messages =
        List.mapi(
          (i: int, msg: Model.message) =>
            if (i == index) {
              {
                ...msg,
                display:
                  switch (msg.display) {
                  | Some(display) =>
                    Some({
                      ...display,
                      collapsed: !display.collapsed,
                    })
                  | None => None
                  },
              };
            } else if (msg.role == System(AssistantPrompt)
                       && is_prompt_display) {
              {
                ...msg,
                display:
                  switch (msg.display) {
                  | Some(display) =>
                    Some({
                      ...display,
                      collapsed: true,
                    })
                  | None => None
                  },
              };
            } else {
              msg;
            },
          curr_chat.messages,
        );
      let updated_past_chats =
        Id.Map.update(
          curr_chat.id,
          opt_chat =>
            switch (opt_chat) {
            | Some(chat: Model.chat) =>
              Some({
                ...chat,
                messages: updated_messages,
              })
            | None => None
            },
          past_chats,
        );
      resculpt_model(
        ~model,
        ~mode,
        ~updated_past_chats,
        ~chat_id=curr_chat.id,
      )
      |> Updated.return;

    | SwitchChat(chat_id) =>
      let mode = settings.assistant.mode;
      let (past_chats, _) = get_mode_info(mode, model);
      resculpt_model(~model, ~mode, ~updated_past_chats=past_chats, ~chat_id)
      |> Updated.return;
    | FilterLoadingMessages =>
      Model.{
        ...model,
        chat_history: {
          past_tutor_chats:
            Id.Map.map(
              (chat: Model.chat) => {
                {
                  ...chat,
                  messages: filter_chat_messages(chat.messages),
                }
              },
              model.chat_history.past_tutor_chats,
            ),
          past_suggestion_chats:
            Id.Map.map(
              (chat: Model.chat) => {
                {
                  ...chat,
                  messages: filter_chat_messages(chat.messages),
                }
              },
              model.chat_history.past_suggestion_chats,
            ),
          past_composition_chats:
            Id.Map.map(
              (chat: Model.chat) => {
                {
                  ...chat,
                  messages: filter_chat_messages(chat.messages),
                }
              },
              model.chat_history.past_composition_chats,
            ),
        },
      }
      |> Updated.return
    | Lop(index) =>
      // Lop off the messages after the index
      let mode = settings.assistant.mode;
      let (_, curr_chat) = get_mode_info(mode, model);
      let sketch_snapshot =
        List.nth(curr_chat.messages, index).sketch_snapshot;
      switch (sketch_snapshot) {
      | Some(sketch) =>
        let perform_action =
          CodeEditable.Update.Perform(Restore(sketch.editor.state.zipper));
        let cell_action = CellEditor.Update.MainEditor(perform_action);
        let scratch_action = Editors.Update.Scratch(CellAction(cell_action));
        schedule_editor_action(scratch_action);
      | None => ()
      };
      let updated_messages =
        curr_chat.messages |> ListUtil.take_up_to_n(index);
      let updated_chat = {
        ...curr_chat,
        messages: updated_messages,
      };
      update_model_chat_history(
        ~model,
        ~mode,
        ~updated_chat,
        ~awaiting_response=false,
      )
      |> Updated.return;
    }
  | ExternalAPIAction(external_api_action) =>
    switch (external_api_action) {
    | SetLLM(model_info) =>
      {
        ...model,
        external_api_info: {
          ...model.external_api_info,
          set_model_info: model_info,
        },
      }
      |> Updated.return
    | SetAPIKey(api_key) =>
      // Set the available models using the provided API key
      OpenRouter.get_models(~key=api_key, ~handler=response => {
        switch (response) {
        | Some(json) =>
          switch (OpenRouter.parse_models_response(json)) {
          | Some(models_response) =>
            schedule_action(
              ExternalAPIAction(SetListOfLLMs(models_response.data)),
            )
          | None =>
            print_endline("Assistant: failed to parse models response")
          }
        | None =>
          print_endline("Assistant: no response received from OpenRouter API")
        }
      });
      {
        ...model,
        external_api_info: {
          ...model.external_api_info,
          api_key,
        },
      }
      |> Updated.return;
    | SetListOfLLMs(llms) =>
      {
        ...model,
        external_api_info: {
          ...model.external_api_info,
          available_models: llms,
          // set llm as the first model to prevent mismatch between dropdown display and set model
          set_model_info: List.hd(llms),
        },
      }
      |> Updated.return
    }
  | InitializeAssistant => AssistantModel.init() |> Updated.return
  };
};
