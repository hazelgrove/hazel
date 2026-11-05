/* The system prompt for Filbert, the composition agent, one string per
   line. */

let self: list(string);

/* Sections of `self`, for prompts that quote one on its own. */
let identity: list(string);
let session_modes: list(string);
let hazel_language_guide: list(string);
let program_model: list(string);

/* Added on the livelits branch (#2635), before this interface existed. */
let message_channels: list(string);
