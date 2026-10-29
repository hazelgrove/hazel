open Util;
open Haz3lcore;

// A dynamic, runtime converter of our chat messages to a UI-friendly format
// This converts a linear log of messages into something more digestible for the user
module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type user_message = {
    content: string,
    origin_id: Id.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type agent_response_chunk = {
    content: list(Message.Model.t),
    agent_reasoning: list(string),
    tool_results: list(AgentToolResult.tool_result),
    // add workbench info
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type compaction_notice = {
    method: string,
    content: string,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type chunk =
    | UserMessage(user_message)
    | AgentResponseChunk(agent_response_chunk)
    | CompactionNotice(compaction_notice)
    | ErrorMessage(string)
    | /** Stopped in-flight LLM/compaction — not under Filbert (see ChunkedUIChat.Utils.mk). */
      ResponseCancelledMessage(
        string,
      )
    | /** Inline output of a chat slash command (/cost, /credits, /usage, /help). UI-only. */
      SlashCommandOutputMessage(
        Message.Model.slash_command_payload,
      );

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    prompt: string,
    developer_notes: string,
    context: string,
    log: list(chunk),
  };
};

module Utils = {
  let mk_user_message_chunk = (message: Message.Model.t): Model.chunk => {
    UserMessage({
      content: message.content,
      origin_id: message.id,
    });
  };

  let mk_agent_response_chunk = (message: Message.Model.t): Model.chunk => {
    AgentResponseChunk({
      content: [message],
      agent_reasoning: [],
      tool_results: [],
    });
  };

  let init = (): Model.t => {
    {
      prompt: "",
      developer_notes: "",
      context: "",
      log: [],
    };
  };

  let curr_last_chunk = (model: Model.t): option(Model.chunk) =>
    model.log |> List.rev |> ListUtil.hd_opt;

  /* Extend the agent response in progress, or open one: a chat may begin
     with an agent message (a replayed run, a chat trimmed by compaction),
     so no message may assume a chunk before it. */
  let with_agent_chunk =
      (
        model: Model.t,
        f: Model.agent_response_chunk => Model.agent_response_chunk,
      )
      : Model.t =>
    switch (curr_last_chunk(model)) {
    | Some(AgentResponseChunk(chunk)) => {
        ...model,
        log:
          (model.log |> List.rev |> List.tl |> List.rev)
          @ [Model.AgentResponseChunk(f(chunk))],
      }
    | _ => {
        ...model,
        log:
          model.log
          @ [
            Model.AgentResponseChunk(
              f({
                content: [],
                agent_reasoning: [],
                tool_results: [],
              }),
            ),
          ],
      }
    };

  let mk = (chat: Chat.Model.t): Model.t => {
    // Converts a list of messages into a list of displayable chunks.
    // The algorithm is roughly as follows:
    /*
     1. Iterate through the log of messages
       1.1 If the message is a user message, create a user message chunk
       1.2 If the message is an agent response, create an agent response chunk
     */

    let rec convert_helper =
            (chat: list(Message.Model.t), acc_model: Model.t): Model.t => {
      switch (chat) {
      | [] => acc_model
      | [message, ...rest] =>
        switch (message.role) {
        | User =>
          let chunk = mk_user_message_chunk(message);
          let updated_model = {
            ...acc_model,
            log: acc_model.log @ [chunk],
          };
          convert_helper(rest, updated_model);
        | Agent(_) =>
          convert_helper(
            rest,
            with_agent_chunk(acc_model, c =>
              {
                ...c,
                content: c.content @ [message],
              }
            ),
          )
        | System(Prompt) =>
          let updated_model = {
            ...acc_model,
            prompt: message.content,
          };
          convert_helper(rest, updated_model);
        | System(DeveloperNotes) =>
          let updated_model = {
            ...acc_model,
            developer_notes: message.content,
          };
          convert_helper(rest, updated_model);
        | System(Context) =>
          let updated_model = {
            ...acc_model,
            context: message.content,
          };
          convert_helper(rest, updated_model);
        | ToolResult(tool_result) =>
          convert_helper(
            rest,
            with_agent_chunk(acc_model, c =>
              {
                ...c,
                content: c.content @ [message],
                tool_results: c.tool_results @ [tool_result],
              }
            ),
          )
        | System(ApiFailure) =>
          let chunk = Model.ErrorMessage(message.content);
          let updated_model = {
            ...acc_model,
            log: acc_model.log @ [chunk],
          };
          convert_helper(rest, updated_model);
        | System(CompactionSummary(method)) =>
          let chunk =
            Model.CompactionNotice({
              method,
              content: message.content,
            });
          let updated_model = {
            ...acc_model,
            log: acc_model.log @ [chunk],
          };
          convert_helper(rest, updated_model);
        | System(RetryNote) =>
          convert_helper(
            rest,
            with_agent_chunk(acc_model, c =>
              {
                ...c,
                content: c.content @ [message],
              }
            ),
          )
        | System(ResponseCancelled) =>
          let chunk = Model.ResponseCancelledMessage(message.content);
          let updated_model = {
            ...acc_model,
            log: acc_model.log @ [chunk],
          };
          convert_helper(rest, updated_model);
        | System(SlashCommandOutput(payload)) =>
          let chunk = Model.SlashCommandOutputMessage(payload);
          let updated_model = {
            ...acc_model,
            log: acc_model.log @ [chunk],
          };
          convert_helper(rest, updated_model);
        }
      };
    };

    let chat = Chat.Utils.get(chat);
    let res = init();

    convert_helper(chat, res);
  };
};
