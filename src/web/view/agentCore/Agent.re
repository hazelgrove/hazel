/* Agent chat: UI roles vs API payload ([[Message.Model.api_message]])

   Policy for what we send to the LLM (see also [[CompositionPrompt.message_channels]]):

   - **System (prompt, context, compaction summary)** — Use [[OpenRouter.Message.Utils.mk_system_msg]].
     Hazel's "developer notes" use [[mk_developer_msg]], which serializes as **system** in
     [[OpenRouter.Message.Utils.json_of_message]] (there is no separate developer role on the wire).

   - **Context snapshot** ([[Message.Utils.mk_context_message]]) — One structured message: program
     view, static/type diagnostics inside `<staticErrorsInfo>`, tests, workbench. Refresh in place
     via [[Chat.Utils.update_context]]; do **not** emit separate "linter" system messages per error.
     Future language-service feedback should use extra tagged blocks here, same cadence.

   - **Tool results** — [[OpenRouter.Message.Utils.mk_tool_msg]]; short, actionable success/failure text.

   - **Mandatory protocol nudges** (empty reply, open subtask) — [[Message.Utils.mk_retry_note_message]]
     with [deliver_as_user_on_api]: UI stays [[System(RetryNote)]], API uses **user** plus an explicit
     prefix so the model attends without confusing the human's messages.

   - **UI-only** — e.g. [[mk_api_failure_message]] with [api_message: None]: never sent to the API.

   OpenRouter wire roles: system, user, assistant, tool (see [[OpenRouter.Message.Model.role]]).
   */

include AgentModel;

module ToolUtils = AgentToolUtils;
module Utils = AgentUtils;
module ToolCallHandler = AgentToolCallHandler;
module Update = AgentUpdate;
