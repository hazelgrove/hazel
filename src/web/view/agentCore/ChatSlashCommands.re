/** Slash commands in the chat input (see ChatBottomBar). Alphabetically ordered names. */
let all_alphabetical: list((string, string)) = [
  ("account-usage", "Show your OpenRouter account credit balance"),
  ("compact", "Summarize the conversation"),
  ("help", "List available slash commands"),
  (
    "jev-builds",
    "Toggle Jev building code without a sketch in jev_edit (experiment)",
  ),
  (
    "jev-edit-tool",
    "Toggle the agent's jev_edit tool: it plans, Jev writes code (experiment)",
  ),
  (
    "jev-mode",
    "Toggle all Jev experiments at once: agent specifies, Jev navigates and builds the code",
  ),
  (
    "jev-prepass",
    "Toggle Jev view pre-selection on each message (experiment)",
  ),
  (
    "jev-view-tool",
    "Toggle the agent's modify_view tool, backed by Jev (experiment)",
  ),
  ("key", "Show the currently-set OpenRouter API key"),
  ("key-usage", "Show usage and limits for the active OpenRouter key"),
  ("session-usage", "Estimate $ cost of this chat from token usage"),
  ("show-thinking", "Toggle display of agent thinking/reasoning text"),
];

let filtered = (filter: string): list((string, string)) => {
  let f = String.lowercase_ascii(filter);
  all_alphabetical
  |> List.filter(((name, _)) =>
       String.length(f) == 0
       || String.starts_with(~prefix=f, String.lowercase_ascii(name))
     );
};

/** Typed payload for the /help command — entries are rendered into a custom card. */
let help_payload = (): Message.Model.help_output => {
  let entries: list(Message.Model.help_entry) =
    List.map(
      ((name, description)): Message.Model.help_entry =>
        {
          help_name: name,
          help_description: description,
        },
      all_alphabetical,
    );
  {help_entries: entries};
};
