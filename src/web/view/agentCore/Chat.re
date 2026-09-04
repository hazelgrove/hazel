open Util_web;
open Util_web.API;
open Haz3lcore;
open AgentResult;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type current_view =
    | Messages
    | Workbench
    | AgentEditorView
    | StaticErrors
    | Prompt
    | DeveloperNotes
    | Tools;

  // Chats are a tree of messages
  // The user can branch off from the current or past message and create threads
  // The current thread is made via linearizing the branch denoted by the current head
  // The user can always toggle between branches in this tree via changing the child pointer

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    id: Id.t,
    title: string,
    message_map: Id.Map.t(Message.Model.t),
    root: Id.t,
    agent_view: AgentContext.Model.t,
    agent_workbench: AgentWorkbench.Model.t, // The agent's todo lists/workbench associated with this chat
    context: option(Message.Model.t),
    created_at: float,
    current_view,
    [@yojson.default []]
    pending_send_queue: list(string),
  };
};

module Utils = {
  let find_message_opt = (id: Id.t, chat: Model.t): option(Message.Model.t) => {
    Id.Map.find_opt(id, chat.message_map);
  };

  let find_message = (id: Id.t, chat: Model.t): Message.Model.t => {
    find_message_opt(id, chat)
    |> OptUtil.get_or_fail(
         "[Chat.Utils.find_message] Message not found from the message map",
       );
  };

  let linearize = (chat: Model.t): list(Message.Model.t) => {
    // Linearizes the the chat model into a list of current messages
    // via recursively traversing the tree from the root through current child nodes
    let rec go =
            (msg: Message.Model.t, messages: list(Message.Model.t))
            : list(Message.Model.t) => {
      switch (msg.current_child) {
      | None => messages
      | Some(child_id) =>
        let child_msg = find_message(child_id, chat);
        go(child_msg, messages @ [child_msg]);
      };
    };
    let root_msg = find_message(chat.root, chat);
    go(find_message(chat.root, chat), [root_msg]);
  };

  let api_messages_of_messages =
      (messages: list(Message.Model.t)): list(OpenRouter.Message.Model.t) => {
    List.filter_map(Message.Utils.api_message_of_message, messages);
  };

  let json_of_messages =
      (messages: list(Message.Model.t), model_id: option(string)): Json.t => {
    `Assoc([
      (
        "model_id",
        switch (model_id) {
        | Some(id) => `String(id)
        | None => `Null
        },
      ),
      (
        "messages",
        `List(List.map(Message.Utils.json_of_message, messages)),
      ),
    ]);
  };

  let current_tail = (chat: Model.t): Message.Model.t => {
    // Calling linearize makes this a little costly
    // but it also keeps code safer rather than storing a current tail pointer
    let linear = linearize(chat);
    ListUtil.hd_opt(List.rev(linear))
    |> OptUtil.get_or_fail(
         "[Chat.Utils.current_tail] Failed to get current tail",
       );
  };

  let parent_of = (message_id: Id.t, chat: Model.t): Message.Model.t => {
    // Searches through all messages in the chat and checks if message_id
    // is in the list of children of any message
    // Note: It should hold that the caller knows the passed message id will have a parent
    let parent =
      List.find_map(
        (message: Message.Model.t) =>
          if (List.mem(message_id, message.children)) {
            Some(message);
          } else {
            None;
          },
        linearize(chat),
      );
    parent
    |> OptUtil.get_or_fail(
         "[Chat.Utils.parent_of] No parent exists for message: "
         ++ Id.to_string(message_id),
       );
  };

  let update_message = (message: Message.Model.t, chat: Model.t): Model.t => {
    {
      ...chat,
      message_map: Id.Map.add(message.id, message, chat.message_map),
    };
  };

  let append = (new_msg: Message.Model.t, chat: Model.t): Model.t => {
    // Inserts a new message at the current tail of the chat

    // Add new message's id to the current tail message's children
    let tail_msg = current_tail(chat);
    let updated_tail_msg = Message.Utils.add_child(tail_msg, new_msg.id);
    let updated_chat = update_message(updated_tail_msg, chat);

    // Add the new message to the chat
    update_message(new_msg, updated_chat);
  };

  let overwrite_message =
      (message_id: Id.t, message: Message.Model.t, chat: Model.t): Model.t => {
    // Like append, but instead of appending, simply overwrites the message with the new one
    // This is helpful, for example, if an api error occurs after we generate an agent response.
    let updated_message_map =
      Id.Map.add(message_id, message, chat.message_map);
    {
      ...chat,
      message_map: updated_message_map,
    };
  };

  let truncate = (id: Id.t, chat: Model.t): Model.t => {
    let message = find_message(id, chat);
    // This effecively, forcefully, sets this msg as the new tail
    let updated_message = {
      ...message,
      current_child: None,
    };
    update_message(updated_message, chat);
  };

  let switch_branch =
      (~fork_id: Id.t, ~new_child_id: Id.t, chat: Model.t): Model.t => {
    let fork_msg = find_message(fork_id, chat);
    let updated_fork_msg = {
      ...fork_msg,
      current_child: Some(new_child_id),
    };
    update_message(updated_fork_msg, chat);
  };

  let update_context =
      (
        ~session_mode: AgentGlobals.Model.session_mode,
        agent_editor_view: string,
        static_errors_info: string,
        test_results_info: string,
        chat: Model.t,
      )
      : Model.t => {
    let workbench =
      Message.Utils.mk_context_message(
        ~session_mode,
        agent_editor_view,
        static_errors_info,
        test_results_info,
        AgentWorkbench.Utils.MainUtils.active_task_to_pretty_string(
          chat.agent_workbench,
        ),
      );
    {
      ...chat,
      context: Some(workbench),
    };
  };

  let append_to_message_content =
      (message_id: Id.t, content: string, chat: Model.t): Model.t => {
    switch (find_message_opt(message_id, chat)) {
    | Some(message) =>
      let updated_message = {
        ...message,
        content: message.content ++ content,
      };
      update_message(updated_message, chat);
    | None => chat
    };
  };

  let get = (chat: Model.t): list(Message.Model.t) => {
    let linear = linearize(chat);
    switch (chat.context) {
    | Some(context) => linear @ [context]
    | None => linear
    };
  };

  let rec last_compaction_index =
          (i: int, acc: option(int), msgs: list(Message.Model.t))
          : option(int) => {
    switch (msgs) {
    | [] => acc
    | [m, ...rest] =>
      let acc' =
        switch (m.role) {
        | Message.Model.System(Message.Model.CompactionSummary(_)) => Some(i)
        | _ => acc
        };
      last_compaction_index(i + 1, acc', rest);
    };
  };

  /** Transcript segment to send to the compaction LLM: after dev notes; if a prior compaction
      summary exists on this branch, **include** that message so the model can merge it with newer
      turns (slice starts at that summary, not after it). */
  let dialogue_slice_for_compaction_summary =
      (chat: Model.t): list(Message.Model.t) => {
    let linear = linearize(chat);
    let start =
      switch (last_compaction_index(0, None, linear)) {
      | None => 2
      | Some(i) => i
      };
    ListUtil.remove_first_n(start, linear);
  };

  /** OpenRouter payload for the main agent: always prompt + dev notes + active suffix from latest compaction, then context. */
  let messages_for_openrouter = (chat: Model.t): list(Message.Model.t) => {
    let linear = linearize(chat);
    let trimmed =
      switch (last_compaction_index(0, None, linear)) {
      | None => linear
      | Some(idx) =>
        switch (linear) {
        | [p, d, ..._] =>
          let suffix = ListUtil.remove_first_n(idx, linear);
          [p, d, ...suffix];
        | _ => linear
        }
      };
    switch (chat.context) {
    | Some(context) => trimmed @ [context]
    | None => trimmed
    };
  };

  /** OpenRouter messages with the advancing prompt-cache breakpoint:
      [cache_anchor] on the last history message (before the context snapshot,
      if any). Currently a no-op on OpenRouter+Anthropic, which honors
      cache_control only on system messages; kept correct-and-ready for
      native-Anthropic routing. See agent-docs/prompt-caching-findings.md. */
  let api_messages_for_openrouter =
      (chat: Model.t): list(OpenRouter.Message.Model.t) => {
    let msgs = api_messages_of_messages(messages_for_openrouter(chat));
    let n = List.length(msgs);
    let anchor_idx = Option.is_some(chat.context) ? n - 2 : n - 1;
    if (anchor_idx < 0) {
      msgs;
    } else {
      List.mapi(
        (i, m: OpenRouter.Message.Model.t) =>
          i == anchor_idx
            ? {
              ...m,
              cache_anchor: true,
            }
            : m,
        msgs,
      );
    };
  };

  /** Value for the context meter: **only** the provider-reported [[prompt_tokens]] from the last
      assistant message in the transcript when that message is still “current” for the bar:
      - No compaction on the branch → last agent message’s usage (if any).
      - Compaction present → that usage **only** if the last agent message is **after** the latest
        compaction summary (same request shape the API already counted). Otherwise [None] so the UI
      shows “—” (no client-side estimates). */
  let context_meter_prompt_tokens = (chat: Model.t): option(int) => {
    let messages = get(chat);
    let (_, last_agent_idx, last_agent_tokens, last_compaction_idx) =
      List.fold_left(
        (
          (i, last_agent_idx, last_agent_tokens, last_compaction_idx),
          msg: Message.Model.t,
        ) => {
          let last_agent_idx' =
            switch (msg.role) {
            | Message.Model.Agent(Some(_)) => Some(i)
            | _ => last_agent_idx
            };
          let last_agent_tokens' =
            switch (msg.role) {
            | Message.Model.Agent(Some(u)) => Some(u.prompt_tokens)
            | _ => last_agent_tokens
            };
          let last_compaction_idx' =
            switch (msg.role) {
            | Message.Model.System(Message.Model.CompactionSummary(_)) =>
              Some(i)
            | _ => last_compaction_idx
            };
          (i + 1, last_agent_idx', last_agent_tokens', last_compaction_idx');
        },
        (0, None, None, None),
        messages,
      );
    switch (last_compaction_idx, last_agent_idx, last_agent_tokens) {
    | (Some(ci), Some(ai), Some(tok)) when ai > ci => Some(tok)
    | (Some(_), _, _) => None
    | (None, _, Some(tok)) => Some(tok)
    | (None, _, None) => None
    };
  };

  let init = (~system_prompt: string, ~dev_notes: string): Model.t => {
    let system_prompt = Message.Utils.mk_prompt_message(system_prompt);
    let chat: Model.t = {
      id: Id.mk(),
      title: "New Chat",
      message_map: Id.Map.singleton(system_prompt.id, system_prompt),
      root: system_prompt.id,
      agent_view: AgentContext.Utils.init(),
      agent_workbench: AgentWorkbench.Utils.MainUtils.init(),
      context: None,
      created_at: JsUtil.timestamp(),
      current_view: Messages,
      pending_send_queue: [],
    };
    let dev_notes = Message.Utils.mk_developer_notes_message(dev_notes);
    let chat = append(dev_notes, chat);
    chat;
  };
};

module Update = {
  module Action = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | AppendMessage(Message.Model.t)
      | SwitchBranch(Id.t, Id.t)
      | BranchOff(Id.t)
      | AgentContextAction(AgentContext.Update.action)
      | WorkbenchAction(AgentWorkbench.Update.Action.action)
      | UpdateContext(AgentGlobals.Model.session_mode, string, string, string)
      | AppendToMessageContent(Id.t, string)
      | OverwriteMessage(Id.t, Message.Model.t)
      | SwitchView(Model.current_view)
      | MessageAction(Id.t, Message.Update.action)
      | SetTitle(string);
  };

  let update = (action: Action.t, model: Model.t): Result.t(Model.t) => {
    switch (action) {
    | SetTitle(title) =>
      Ok({
        ...model,
        title,
      })
    | AppendMessage(message) => Ok(Utils.append(message, model))
    | SwitchBranch(fork_id, new_child_id) =>
      Ok(Utils.switch_branch(~fork_id, ~new_child_id, model))
    | BranchOff(fork_id) =>
      let truncated_model = Utils.truncate(fork_id, model);
      Ok(truncated_model);
    | AgentContextAction(agent_context_action) =>
      Ok({
        ...model,
        agent_view:
          AgentContext.Update.update(agent_context_action, model.agent_view),
      })
    | WorkbenchAction(workbench_action) =>
      let workbench =
        AgentWorkbench.Update.update(
          ~model=model.agent_workbench,
          ~action=workbench_action,
        );
      switch (workbench) {
      | Success(updated_workbench) =>
        Ok({
          ...model,
          agent_workbench: updated_workbench,
        })
      | Failure(error) => Error(Failure.Info(error))
      };
    | UpdateContext(
        session_mode,
        agent_editor_view,
        static_errors_info,
        test_results_info,
      ) =>
      Ok(
        Utils.update_context(
          ~session_mode,
          agent_editor_view,
          static_errors_info,
          test_results_info,
          model,
        ),
      )
    | AppendToMessageContent(message_id, content) =>
      Ok(Utils.append_to_message_content(message_id, content, model))
    | OverwriteMessage(message_id, message) =>
      Ok(Utils.overwrite_message(message_id, message, model))
    | SwitchView(current_view) =>
      Ok({
        ...model,
        current_view,
      })
    | MessageAction(message_id, message_action) =>
      let message = Utils.find_message(message_id, model);
      let updated_message = Message.Update.update(message_action, message);
      Ok(Utils.update_message(updated_message, model));
    };
  };
};
