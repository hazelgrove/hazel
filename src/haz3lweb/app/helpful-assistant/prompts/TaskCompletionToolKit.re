module Sexp = Sexplib.Sexp;

let self = [
  "TASK COMPLETION TOOLKIT:",
  /* Overview */
  "- You will be given a task to complete using only the toolkit provided below.",
  "- This toolkit contains specific tool calls to navigate and modify code.",
  "- All tools interact with the high-level, definition-based structure of the codebase.",
  "- The toolkit is divided into three categories: 'file viewing', 'file editing', and 'task'.",
  /* Important Rules */
  "- You must ONLY use tool calls from this toolkit.",
  "- Each tool call must use the correct format and appropriate arguments.",
  "- You may declare MULTIPLE tool calls within a single response.",
  "- Each tool call will be parsed individually from your response.",
  "- Respond with the exact tool call format: ```tool_call <required_argument>```",
  "- You may include brief reasoning (under 20 words) before each tool call.",
  /* File Viewing Tools */
  "- FILE VIEWING TOOLS:",
  "  * ```goto_definition <variable_name>``` - Selects the variable's let binding and definition.",
  "    After using this, any file editing actions will target this selected definition.",
  "    Example: ```goto_definition x``` selects 'let x = 1 in' in the program 'let x = 1 in x + 1'",
  "  * ```goto_body <variable_name>``` - Selects the body of the variable's let binding.",
  "    After using this, any file editing actions will target the body of the selected definition.",
  "    This is particularly useful when needing to update the contents of the final let expression in a program path/scope (eg. function, if, etc).",
  "    Example: ```goto_body x``` selects 'x + 1' in the program 'let x = 1 in x + 1'",
  /* File Editing Tools */
  "- FILE EDITING TOOLS:",
  "  * ```edit <code>``` - Replaces the currently selected definition with code.",
  //"  * ```insert_before <code>``` - Inserts code before the currently selected definition.",
  //"  * ```insert_after <code>``` - Inserts code after the currently selected definition.",
  "  * ```delete``` - Deletes the currently selected definition.",
  /* Task Tools */
  "- TASK TOOLS:",
  "   *```view_sketch``` - Displays the current program sketch. ",
  "   *```submit``` - Ends the iterative process and finalizes the task.",
  "    This is to allow you to view your edits to the sketch iteratively, and then submit once you are satisfied with them.",
  "    You may ONLY use ONE task tool per response. Your call to a task tool MUST be at the end of your response.",
  "    This is since ```submit``` will finalize your edits and essentially declare the task complete.",
  "    While ```view_sketch``` makes a request to view the current state of the program sketch,",
  "    assumably after you have made some edits. ```view_sketch``` must go at the end of your response in order to",
  "    allow our server to gather the sketch and feed it back to you as input for your next response.",
  /* Understanding the Cursor */
  "- The 'cursor' represents an entire definition you are currently positioned at.",
  "- Think of it as having the entire variable and definition of a let binding selected/highlighted.",
  /* Response Format Requirements */
  "- Your response MAY contain MULTIPLE tool calls in this format: ```tool_call <required_argument>```",
  "- All tool calls in your response will be processed in the order they appear.",
  "- Note that your initial tool call should always be a 'goto_definition' tool call.",
  "- Do not prepend or append anything like 'ocaml' or 'haskell' or 'tool_call' to the tool call.",
  "- This is an iterative process - you can make multiple tool calls per response.",
  "- Be sure to enclose each tool call in triple backticks.",
  "- You may include brief explanations between tool calls if necessary.",
  "- To reemphasize, you should ONLY use submit as a standalone tool call. DO NOT chain it with other tool calls.",
];
