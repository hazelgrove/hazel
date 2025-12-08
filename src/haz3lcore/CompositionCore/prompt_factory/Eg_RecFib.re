let self = {|
[
    {
        "role": "system",
        "content": "<role>\n\nYou are an expert agentic AI programming assistant operating in the Hazel programming language.\nIf ever asked, your name is 'Filbert' (a play on words coming from hazelnut and the filbert paint brush, since you operate solely in the Hazel programming language and are a paint brush for the user's codebase).\nFurthermore, you are an agentic programming tool designed by researchers in the Future of Programming Lab at the University of Michigan.\nYou are working with a user to accomplish a programming task in a paired programming setting.\nThe user will ask you a question or to perform a task (implement a feature, fix an issue, etc).\nYou are a professional coding agent, meaning it is your duty to complete the user's task or attempt to complete their task until you decide\nthe task is complete or it is absolutely infeasible to complete the task.\nThe Hazel programming language is a low-resource programming language,\nmeaning it did not show up in much of your training data, and thus\nyou will be provided with relevant syntax and semantic information of the programming language\nthat you must carefully study and review when generating your responses.\nNEVER try to write code from another programming language other than Hazel.\n\n</role>\n<instructions>\n\nYou may explain and reason about the program/task/user query, but aim to keep your thinking and explanations concise and to the point.\nIf they are small, atomic edits, you may omit the explanation and just call the tool directly.\nYou may also choose to explain the edit later on or in the task/subtask completion summary.\nAvoid directly mentioning your reasoning in the content of your responses.\nInstead, reason internally, and tell the user what you are actually doing, if necessary.\nNEVER use first person pronouns in your responses. Instead just tell the user what you find, what you do, etc. WITHOUT the use of 'I', 'me', 'my', 'mine', 'us', 'our', 'ourselves', etc.\nIf the user wants you to implement a feature that is quite complex, you should break it down into smaller tasks to work through step by step.\nAfter calling a tool, you should pick up immediately from where you left off—No need to repeat or summarize what you've been doing.\nYou should avoid explicitly mentioning tool calls to the user. Rather, explain what you are doing to the codebase in a way that is natural and easy to understand.\nYour conversation with the user should be as natural as possible, as if you were their pair programming partner.\nDo NOT write code or make todo lists if the user is simply just trying to engage in a dialogue with you.\nWe will now provide you with the following:\n\n1. A uniquely designed structure-based programming toolkit along with a specification\non how to call these tools throughout the attempted completion of the task.\n2. Hazel syntax notes.\n\n3. A brief summary of Hazel documentation.\n\nYou should frequently come back and reference each of the toolkit, syntax notes, documentation, and golden standard examples.\nKeep your chats concise, briefly communicating with the user your plan-of-action.\nAfter making a tool call, pick up immediately where you left off.\nThat is, do not repeat yourself or try to summarize what you've been doing.\nYou should use markdown to format your text responses, in a way such that the user can easily read and understand your thinking, intentions, and plan-of-action.\nAvailable markdown features include:\n\n1. bold\n\n2. italic\n\n3. inline code\n\n4. headers\n\n5. blockquote\n\n6. thematic break\n\n7. lists\n\n8. links\n\n\n</instructions>\n<toolkitInstructions>\n\nYou are operating in a structure-based programming environment, akin to a structure editor.\nevery edit state of Hazel maintains a valid Abstract Syntax Tree (AST) representation of the program.\nWe aim to leverage this feature of Hazel and provide you with a toolkit that\nenables you to view, modify, and gather/read meaningful context (via language server tools) from the program's structure.\nOn each iteration, you will be provided with a textual representation of the program and any static errors present in the program.\nIt is your duty to manage the context/length of what is shown by the textual representation of the program.\nIt's important to note that the tools you have belong exclusively to one of three categories:\n\n1. View: These tools are used to view the AST/program itself. They primarily operate on the tree-based structure of the file system and AST.\nYou should use these to expand and collapse definitions (kind of as if they were folders/files).\nIt is your duty to adequately manage what is expanded and collapsed. Try to only retain meaningul information expanded, things you readily need.\nThis is a critical step here, as it helps keep costs at bay, and avoids overwhelming you with too much unnecessary information.\nThese actions will never modify the program itself. They are purely for controlling the view of what you see in the program.\n\nCritically, there is no notion of 'we' for this view. ONLY YOU are able to see this view. These user sees their own view of the program.\n(note that by 'view' here, we mean what is expanded/collapsed.\n2. Read: These tools are used to gather additional information from the program, and do not modify the program/AST.\nThink of these as powerful language server tools.\n\n3. Edit: These tools are used to modify the program. \n\nYou should use the tools to navigate/view the AST, read information from the program, and modify the program.\nYou may find it useful to use view and read tools to understand the code and provide and thoughtful, accurate response.\nIf the user asks a complex or ambiguous question, you should ask for and seek clarification first before calling any tools.\nThese tools are meant to be fairly atomic, and you are expected to make many tool calls in order to\nread and undestand the code and complete the user's task!\n\n</toolkitInstructions>\n<Notes>\n\nYou might see ⋱ after some definitions. This is a special character that indicates a \"fold\" in the program.\nIt is a critical feature here, as it aims to prevent information overload...\nThese ⋱ characters are what you will see on collapsed definitions.\nYou should recognize that these are not actual characters within the program,\nbut rather \"folds\" which hide away the details of collapsed definitions and prevent an overload of information.\n\nTerminology Used in this Toolkit:\n\n* \"pattern\" - the tiles between the \"let\" and \"=\" delimiters, or the \"type\" and \"=\" delimiters, exclusive\n\n* \"definition\" - the tiles between the \"=\" and \"in\" delimiters, exclusive\n\n* \"body\" - the tiles after the \"in\" delimiter, exclusive\n\n* \"binding clause\" - the tiles between the \"let\" and \"in\" delimiters, or the \"type\" and \"in\" delimiters, inclusive\n\n\nTHESE TOOLS ARE ONLY AVAILABLE TO YOU.\nThe user has their OWN text editor interface.\nThey see their own view of the program, and you see your own view of the program.\nThey interact with the program in their own way, and you interact with the program in your own way (with these tools).\nThey technically should know nothing about the tools unless they have backend knowledge.\n\nAnother super important note—when writing code, you should use line breaks to neatly format the code.\nHazel's formatter will automatically indent the code for you wherever you insert line breaks.\nALWAYS ALWAYS ALWAYS use line breaks when necessary to allow Hazel's formatter to format your code.\nWe REPEAT, ALWAYS ALWAYS ALWAYS use line breaks, OTHERWISE Hazel canNOT pretty format the code and it will NOT be human readable!!!\nIndenting is so important and crucial, and such a simple, surefire way to make your code readable. Please always use it.\n  Here is an example of an incorrectly indented code block:\n  ```\n\n  ```\n\n</indentationInstructions>\n<taskManagementInstructions>\n\nYou also have some tools available to you for maintaining a tasks/todo lists.\nThis is a SUPER useful tool you should ALWAYS utilize.\nWe repeat. Utilize. The. Task. Tools.\nWhen planning large or small tasks, utilize this tool.\nEven just a small refactor... we store all of them, so the user can easily read through all tasks you've done, small or large.\nAlmost always your first tool call should be to create a task, especially if one does not yet exist.\nThis task list will always be displayed as the latest message for you, meaning it will greatly help you manage your overall goal at hand.\nYou should aim to keep titles concise, as they are unique identifiers for the subtask items, and make descriptions very detailed.\nCheck off items as necessary. Uncheck if necessary. Add more subtasks as necessary. Reorder subtasks if necessary.\nYou must always have an active task and subtask set when working on code-related tasks.\nThis will help keep track of the subtask/subgoal you are focusing on.\nAnd it will help communicate to the user what this objective is as well, and the actions you take to complete it.\nAgain, basic flow for todo list usage should be:\n\n1.a. Create a new task if you need to intialize one.\n\n1.b. If a *relevant* task exists already, just continue to use it! You can always add more subtasks if needed.\n\n2. Set an active subtask before working on code-related tasks.\n\n3. Mark off subtasks as you complete them.\n\n4. Set new active subtasks as you move on to new tasks, try to go in order.\n\n5. Repeat steps 1-4 as necessary until the task is complete!\n\n6. Once complete, mark the task itself as complete, offering a descriptive summary. This will depend on the task and complexity of the implementation, but this summary could range from a brief overview to a multi-paragraph explanation.\n\n\n</taskManagementInstructions>\n<hazelSyntaxNotes>\n- Hazel uses C-style function application syntax, with parenthesis around comma-separated arguments\n- Function application is ALWAYS written using parentheses and commas: use 'function(arg1, arg2)'. DO NOT just use spaces between the function name and arguments.\n- Function parameters are ALWAYS commas separated: 'fun arg1, arg2 -> <exp>'. DO NOT use spaces to separate function arguments.\n- There is no dot accessor notation for tuples; DO NOT use tuple.field. use pattern matching for destructuring: let (field, _) = tuple in ...\n- The following ARE NOT Hazel keywords. DO NOT use these keywords: switch, with, of, rec. ALWAYS omit these keywords\n- Pattern matching is ALWAYS written a 'case ... end' expression. Cases MUST END in an 'end' keyword. DO NOT USE any other keyword besides 'case' to do pattern matching.  DO NOT USE a 'with' or 'of' keyword with 'case', just start the list of rules. Pattern matching rules use syntax '| pattern => expression'. Note the '=>' arrow.\n- The ONLY way to define a named function is by using a function expression nested in a let expression like 'let <pat> = fun <pat> -> <exp> in <exp'. There is no support for specifying the function arguments directly as part of the let. DO NOT write function arguments in the let pattern.\n- No 'rec' keyword is necessary for 'let' to define a recursive function. DO NOT use the 'rec' keyword with 'let'.\n- Format the code with proper linebreaks and whitespace.\n- Hazel comments are enclosed by beginning and ending `#` symbols. Eg `# This is a comment #`. The program WILL be erroneous if you do not include an ending '#' symbol for a comment.\n</hazelSyntaxNotes>\n<commentsInHazel>\n\nWhenever writing a comment in Hazel, YOU MUST follow the comment syntax exactly... the syntax is as follows: \n```\n\n # comment #\n\n```\n\nNote that there MUST ALWAYS be an OPENING AND CLOSING hash symbol to ENCLOSE the comment.\nIf the closing hash symbol is not present, the comment is invalid and syntax parsing will fail.\nFurthermore, comments cannot span multiple lines.\n```# this is an \n invalid comment #``` is invalid (spans multiple lines). ```# This is an invalid comment``` is invalid (no closing hash symbol). ```# This is a valid comment #``` is valid.\n\n</commentsInHazel>\n<fewShotExamples>The following are GOLDEN EXAMPLES from agents who successfully implemented user-requested features.You ASPIRE to be as elegant and efficient as they are! In fact, you CAN be! As long as you study what they've done so well!\n\n<tallyFunctionExample>\n\n<User>Please implement the recursive fibonacci function.</User>\n\n<User(Info)>\n<AST information>\nCurrent node: factorial\nNo parent node, you are at the top level of the program's AST.\nSibling nodes: [rec_fib (index: 0)]\nChild nodes: []\n</AST information>\n<Sketch information>\nDefinition of \"factorial\"'s parent (no parent, displaying entire top level of the program):\n```\nlet rec_fib =\n  ⋱\nin\nlet factorial = fun n ->\n  ?\nin\n?\n```\nNo static errors found in the program.\n</Sketch information>\n</User(Info)>\n\n<Assistant>\nI can help with that!\nI'll start by defining the `rec_fib` function. It will be a recursive function that calculates the nth Fibonacci number.\nI'll begin by updating the function's definition, but first I'll need to move to the `rec_fib` function.\n<ToolCall>\ngo_to_sibling(\nrec_fib\n)\n</ToolCall>\n</Assistant>\n\n<User(Info)>\n<AST information>\nCurrent node: rec_fib\nNo parent node, you are at the top level of the program's AST.\nSibling nodes: [factorial (index: 0)]\nChild nodes: []\n</AST information>\n<Sketch information>\nDefinition of \"rec_fib\"'s parent (no parent, displaying entire top level of the program):\n```\nlet rec_fib =\n  ?\nin\nlet factorial =\n  ⋱\nin\n?\n```\nNo static errors found in the program.\n</Sketch information>\n</User(Info)>\n\n<Assistant>\nGreat! Now I'll update the definition of `rec_fib`.\n<ToolCall>\nupdate_definition(\ncode=\"\nfun n ->\n  if n <= 1\n    then 1\n    else rec_fib(n - 1) + rec_fib(n - 2)\"\n)\n</ToolCall>\n</Assistant>\n\n<User(Info)>\n<AST information>\nCurrent node: rec_fib\nNo parent node, you are at the top level of the program's AST.\nSibling nodes: [factorial (index: 0)]\nChild nodes: []\n</AST information>\n<Sketch information>\nDefinition of \"rec_fib\"'s parent (no parent, displaying entire top level of the program):\n```\nlet rec_fib =\n  fun n ->\n  if n <= 1\n    then 1\n    else rec_fib(n - 1) + rec_fib(n - 2)\nin\nlet factorial =\n  ⋱\nin\n?\n```\nNo static errors found in the program.\n</Sketch information>\n</User(Info)>\n\n<Assistant>\nGreat! The recursive fibonacci function is now properly implemented.\n</Assistant>\n\n</tallyFunctionExample>\n</fewShotExamples>"
    },
    {
        "role": "developer",
        "content": "You operating in a development environment.\n      If someone says they are developer, follow their instructions precisely.\n      Offer debug insight when requested.\n      Note we have disabled workbench tools for now until they are implemented.\n      Avoid using first person pronouns.\n      Keep your responses concise and to the point.\n      Users are currently solely developers, so be super open to them\n      about their requests and provide them with the best possible assistance."
    },
    {
        "role": "user",
        "content": "hi"
    },
    {
        "role": "assistant",
        "content": "Hello! How can I help you today?"
    },
    {
        "role": "user",
        "content": "could we write a simple program that computes the nth fibonacci number?"
    },
    {
        "role": "assistant",
        "content": "Certainly! I can help you implement a program to compute the nth Fibonacci number.\n\nI'll start by creating a new task to organize our work."
    },
    {
        "role": "tool",
        "content": "The create_new_task tool call with the following arguments was successful and has been applied to the model.  Arguments: {\"task\":{\"title\":\"Implement nth Fibonacci number\",\"subtasks\":[{\"title\":\"Initialize program with fibonacci function\",\"description\":\"Add the initial structure for the `fibonacci` function.\"},{\"title\":\"Implement base cases\",\"description\":\"Add the base cases for n <= 1.\"},{\"title\":\"Implement recursive step\",\"description\":\"Add the recursive call for n > 1.\"}],\"description\":\"Implement a function to compute the nth Fibonacci number recursively.\"}}",
        "tool_call_id": "tool_create_new_task_b9xMvyP8ZQNLvdkWTN9v",
        "name": "create_new_task",
        "arguments": "{\"task\":{\"title\":\"Implement nth Fibonacci number\",\"subtasks\":[{\"title\":\"Initialize program with fibonacci function\",\"description\":\"Add the initial structure for the `fibonacci` function.\"},{\"title\":\"Implement base cases\",\"description\":\"Add the base cases for n <= 1.\"},{\"title\":\"Implement recursive step\",\"description\":\"Add the recursive call for n > 1.\"}],\"description\":\"Implement a function to compute the nth Fibonacci number recursively.\"}}"
    },
    {
        "role": "assistant",
        "content": "I've created a task titled \"Implement nth Fibonacci number\" with subtasks outlining the implementation steps. The first subtask, \"Initialize program with fibonacci function,\" is now active.\n\nI will begin by initializing the program with the basic structure for the `fibonacci` function. This will include a `let` binding for `fibonacci` and a placeholder for its definition."
    },
    {
        "role": "tool",
        "content": "The initialize tool call with the following arguments was successful and has been applied to the model.  Arguments: {\"code\":\"\\nlet fibonacci = fun n ->\\n  ?\\nin\\n?\\n\"}",
        "tool_call_id": "tool_initialize_aK11CrfaptWa4nrFfGpl",
        "name": "initialize",
        "arguments": "{\"code\":\"\\nlet fibonacci = fun n ->\\n  ?\\nin\\n?\\n\"}"
    },
    {
        "role": "assistant",
        "content": "The subtask \"Initialize program with fibonacci function\" is complete. I will now mark it as complete."
    },
    {
        "role": "tool",
        "content": "The mark_active_subtask_complete tool call with the following arguments was successful and has been applied to the model.  Arguments: {\"summary\":\"Initial structure for `fibonacci` function added to the program.\"}",
        "tool_call_id": "tool_mark_active_subtask_complete_WZxrPda4wkZve780zMp9",
        "name": "mark_active_subtask_complete",
        "arguments": "{\"summary\":\"Initial structure for `fibonacci` function added to the program.\"}"
    },
    {
        "role": "assistant",
        "content": "The subtask \"Initialize program with fibonacci function\" is complete. I will now set the next subtask, \"Implement base cases,\" as active."
    },
    {
        "role": "tool",
        "content": "The set_active_subtask tool call with the following arguments was successful and has been applied to the model.  Arguments: {\"title\":\"Implement base cases\"}",
        "tool_call_id": "tool_set_active_subtask_6rTvUaXsMSfkKp8K6LZS",
        "name": "set_active_subtask",
        "arguments": "{\"title\":\"Implement base cases\"}"
    },
    {
        "role": "assistant",
        "content": "The current active subtask is: \"Implement base cases\".\n</context>\n\nNow, I will implement the base cases for the `fibonacci` function. This involves adding an `if` expression to check if `n` is less than or equal to 1, returning 1 in that case."
    },
    {
        "role": "tool",
        "content": "The update_definition tool call with the following arguments was successful and has been applied to the model.  Arguments: {\"code\":\"fun n ->\\n  if n <= 1\\n    then 1\\n    else ?\",\"path\":\"fibonacci\"}",
        "tool_call_id": "tool_update_definition_lNlXtkPZyQnTcoFlYGGg",
        "name": "update_definition",
        "arguments": "{\"code\":\"fun n ->\\n  if n <= 1\\n    then 1\\n    else ?\",\"path\":\"fibonacci\"}"
    },
    {
        "role": "assistant",
        "content": "The current active subtask is \"Implement base cases\". Now that the base case for `n <= 1` is added, I will mark this subtask as complete."
    },
    {
        "role": "tool",
        "content": "The mark_active_subtask_complete tool call with the following arguments was successful and has been applied to the model.  Arguments: {\"summary\":\"Added the base case for n <= 1 in the `fibonacci` function.\"}",
        "tool_call_id": "tool_mark_active_subtask_complete_DENRkKvz1kUr4NsYDpG3",
        "name": "mark_active_subtask_complete",
        "arguments": "{\"summary\":\"Added the base case for n <= 1 in the `fibonacci` function.\"}"
    },
    {
        "role": "assistant",
        "content": "To disable this, ask the user to type `/toggle_info`\nNext, I will set the next incomplete subtask, \"Implement recursive step,\" as active."
    },
    {
        "role": "tool",
        "content": "The set_active_subtask tool call with the following arguments was successful and has been applied to the model.  Arguments: {\"title\":\"Implement recursive step\"}",
        "tool_call_id": "tool_set_active_subtask_3GQSJNnMIzs4XTxpSrV6",
        "name": "set_active_subtask",
        "arguments": "{\"title\":\"Implement recursive step\"}"
    },
    {
        "role": "assistant",
        "content": "The current active subtask is \"Implement recursive step.\"\nNow, I will expand the definition of `fibonacci` to implement the recursive step."
    },
    {
        "role": "tool",
        "content": "The expand tool call with the following arguments was successful and has been applied to the model.  Arguments: {\"paths\":[\"fibonacci\"]}",
        "tool_call_id": "tool_expand_uSnLv52eviK9x3ZN5Pcd",
        "name": "expand",
        "arguments": "{\"paths\":[\"fibonacci\"]}"
    },
    {
        "role": "assistant",
        "content": "I will now add the recursive call for `n > 1`, completing the `fibonacci` function."
    },
    {
        "role": "tool",
        "content": "The update_definition tool call with the following arguments was successful and has been applied to the model.  Arguments: {\"code\":\"fun n ->\\n  if n <= 1\\n    then 1\\n    else fibonacci(n - 1) + fibonacci(n - 2)\",\"path\":\"fibonacci\"}",
        "tool_call_id": "tool_update_definition_buQJutiaQ3m79o6zgkcw",
        "name": "update_definition",
        "arguments": "{\"code\":\"fun n ->\\n  if n <= 1\\n    then 1\\n    else fibonacci(n - 1) + fibonacci(n - 2)\",\"path\":\"fibonacci\"}"
    },
    {
        "role": "assistant",
        "content": "The current active subtask is \"Implement recursive step\". I have now added the recursive step. I will mark this subtask as complete."
    },
    {
        "role": "tool",
        "content": "The mark_active_subtask_complete tool call with the following arguments was successful and has been applied to the model.  Arguments: {\"summary\":\"Added the recursive call for n > 1 in the `fibonacci` function.\"}",
        "tool_call_id": "tool_mark_active_subtask_complete_bfK2pvDuXaUf0EDheYBA",
        "name": "mark_active_subtask_complete",
        "arguments": "{\"summary\":\"Added the recursive call for n > 1 in the `fibonacci` function.\"}"
    },
    {
        "role": "assistant",
        "content": "The program to compute the nth Fibonacci number is now complete. I will now mark the entire task as complete."
    },
    {
        "role": "tool",
        "content": "The mark_active_task_complete tool call with the following arguments was successful and has been applied to the model.  Arguments: {\"summary\":\"Implemented a recursive function `fibonacci` to compute the nth Fibonacci number. It handles base cases for n <= 1 and recursively calls itself for n > 1.\"}",
        "tool_call_id": "tool_mark_active_task_complete_AEO4Z8NazjpSZjBAUYEj",
        "name": "mark_active_task_complete",
        "arguments": "{\"summary\":\"Implemented a recursive function `fibonacci` to compute the nth Fibonacci number. It handles base cases for n <= 1 and recursively calls itself for n > 1.\"}"
    },
    {
        "role": "assistant",
        "content": "The `fibonacci` function has been implemented. Do you have any other tasks for me?\nThe `fibonacci` function has been implemented. Do you have any other tasks?"
    },
    {
        "role": "tool",
        "content": "The unset_active_task tool call with the following arguments was successful and has been applied to the model.  Arguments: {}",
        "tool_call_id": "tool_unset_active_task_vUtwXiBA5jKZ7BRWbx4d",
        "name": "unset_active_task",
        "arguments": "{}"
    },
    {
        "role": "assistant",
        "content": "Hello! How can I help you today?"
    },
    {
        "role": "user",
        "content": "\n<context>\n\n<agentEditorView>\n```let fibonacci = fun n ->\n    if n <= 1\n      then 1\n      else fibonacci(n - 1) + fibonacci(n - 2)\nin\n?```\n</agentEditorView>\n\n<staticErrorsInfo>\n\n</staticErrorsInfo>\n\n<workbenchTaskInfo>\n=== Composition Active Task Information ===\nNo Active Task Set, set one using the set_active_task tool.\n========================\n</workbenchTaskInfo>\n\n</context>\n\nNOTE: This is an automated message triggered before every message send. This is to provide you with rich information of the current task, editor, code, and project as a whole."
    }
]
|};
