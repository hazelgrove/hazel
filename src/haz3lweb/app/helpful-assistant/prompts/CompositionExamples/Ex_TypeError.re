module Sexp = Sexplib.Sexp;

let self = [
  "Example: Fixing a simple type error in a function",
  "USER MESSAGE: please fix my program",
  "SYSTEM MESSAGE: PROGRAM SKETCH: ```# Simple function to add input to \"hello\" #
    let add: Int -> Int =
    fun x -> x + \"hello\" in
    # Test out my function #
    add(5)```
    STATIC ERRORS:
    Error in term:
    \"hello\"
    Nature of error: Expecting type Int but got inconsistent type String
    SELECTED CODE: None. Use a goto_* command to select a code segment.",
  "LLM MESSAGE: I'll fix the type error in the add function by first locating its definition.

    {{{goto_definition add}}}

    Now I'll correct the type issue by ensuring both operands are integers.

    {{{edit let add: Int -> Int = fun x -> x + 5 in}}}

    Let me see how those changes impact the program.

    {{{view_sketch}}}",
  "SYSTEM MESSAGE: You have requested to view the sketch. Please review and continue with completing the user-specified task.
    PROGRAM SKETCH: ```# Simple function to add input to \"hello\" #
    let add: Int -> Int = fun x -> x + 5 in
    # Test out my function #
    add(5)```
    STATIC ERRORS:
    No static errors found
    SELECTED CODE: None. Use a goto_* command to select a code segment.
    ",
  "LLM MESSAGE: The function now correctly adds an integer to the input. Task completed:

    {{{submit}}}",
];
