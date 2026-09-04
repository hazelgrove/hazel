open Util_web;
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

  let curr_last_chunk = (model: Model.t): Model.chunk => {
    model.log
    |> List.rev
    |> ListUtil.hd_opt
    |> OptUtil.get_or_fail("No last chunk found");
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
          switch (curr_last_chunk(acc_model)) {
          | AgentResponseChunk(agent_response_chunk) =>
            let agent_response_chunk = {
              ...agent_response_chunk,
              content: agent_response_chunk.content @ [message],
            };
            let log =
              (acc_model.log |> List.rev |> List.tl |> List.rev)
              @ [Model.AgentResponseChunk(agent_response_chunk)];
            let acc_model = {
              ...acc_model,
              log,
            };
            convert_helper(rest, acc_model);
          | _ =>
            let chunk = mk_agent_response_chunk(message);
            let updated_model = {
              ...acc_model,
              log: acc_model.log @ [chunk],
            };
            convert_helper(rest, updated_model);
          }
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
          let curr_last_chunk = curr_last_chunk(acc_model);
          let updated_agent_chunk =
            switch (curr_last_chunk) {
            | AgentResponseChunk(agent_response_chunk) => {
                ...agent_response_chunk,
                content: agent_response_chunk.content @ [message],
                tool_results:
                  agent_response_chunk.tool_results @ [tool_result],
              }
            | _ => failwith("Expected AgentResponseChunk before ToolResult")
            };
          let updated_log =
            (acc_model.log |> List.rev |> List.tl |> List.rev)
            @ [Model.AgentResponseChunk(updated_agent_chunk)];
          let updated_model = {
            ...acc_model,
            log: updated_log,
          };
          convert_helper(rest, updated_model);
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
          switch (curr_last_chunk(acc_model)) {
          | AgentResponseChunk(agent_response_chunk) =>
            let agent_response_chunk = {
              ...agent_response_chunk,
              content: agent_response_chunk.content @ [message],
            };
            let log =
              (acc_model.log |> List.rev |> List.tl |> List.rev)
              @ [Model.AgentResponseChunk(agent_response_chunk)];
            let updated_model = {
              ...acc_model,
              log,
            };
            convert_helper(rest, updated_model);
          | UserMessage(_)
          | CompactionNotice(_)
          | ErrorMessage(_)
          | ResponseCancelledMessage(_)
          | SlashCommandOutputMessage(_) =>
            let chunk = mk_agent_response_chunk(message);
            let updated_model = {
              ...acc_model,
              log: acc_model.log @ [chunk],
            };
            convert_helper(rest, updated_model);
          }
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
