/* The system prompt for Filbert, the composition agent, as one string per
   line -- AgentUtils joins it with newlines. Sections are separate values in
   the implementation so they can be reordered and read on their own; `self`
   fixes the order they reach the model in. */

let self: list(string);

/* Sections of `self` that the compaction prompt quotes on their own. */
let identity: list(string);
let session_modes: list(string);
let hazel_language_guide: list(string);
let program_model: list(string);
