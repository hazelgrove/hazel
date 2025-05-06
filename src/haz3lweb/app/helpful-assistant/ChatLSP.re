open Util;
open Haz3lcore.Info;
open Haz3lcore;
open OptUtil.Syntax;

module Options = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    params: OpenRouter.params,
    instructions: bool,
    syntax_notes: bool,
    num_examples: int,
    expected_type: bool,
    relevant_ctx: bool,
    error_rounds_max: int,
  };

  let init: t = {
    params: OpenRouter.default_params,
    instructions: true,
    syntax_notes: true,
    num_examples: 9,
    expected_type: true,
    relevant_ctx: true,
    error_rounds_max: 2,
  };
};

module SystemPrompt = {
  let prelude = [
    "You are a helpful coding assistant in Hazel that will help the user fill holes whenever they type '?a' or '??' in a hole. \n",
  ];

  let main_prompt = (completion_token: string) => [
    "- CODE COMPLETION INSTRUCTIONS:",
    "- Reply with a functional, idiomatic replacement for the program hole marked '"
    ++ completion_token
    ++ "' in the provided program sketch",
    "- The program hole marked '"
    ++ completion_token
    ++ "' consists of the hole token '??' appended with a series of characters to help you differentiate between other holes in the program",
    // Uncomment either of the following lines to test error rounds
    // "- However, make sure that your initial response CAUSES A TYPE ERROR in the program. Then, fix it in your second response",
    // "- However, it is CRITICALLY important to make sure that your response ALWAYS CAUSES A TYPE ERROR in the program, no matter how many times you are re-prompted",
    "- Reply ONLY with a SINGLE replacement term for the unqiue distinguished hole marked '"
    ++ completion_token
    ++ "'",
    "- Reply ONLY with code",
    "- DO NOT suggest more replacements for other holes in the sketch (marked, '?', '??', or '?a'), or implicit holes",
    "- This is critical, and I am going to reiterate it: DO NOT suggest more than one replacement term. It should ONLY be for the hole marked '"
    ++ completion_token
    ++ "'",
    "- For example, if you are being asked to complete 'let f = ? in "
    ++ completion_token
    ++ "', your response should ONLY be a single replacement term for the hole marked '"
    ++ completion_token
    ++ "', NOT a replacement term for the hole marked '?'",
    "- i.e. You should ONLY respond with a function application, or something else which would be a valid replacement term for the hole marked '"
    ++ completion_token
    ++ "'",
    "- If you wish to include a hole in your response, use '??' only, without the appended characters that were used to identify the specific hole you were given",
    "- IT WOULD BE A HUGE MISTAKE TO RESPOND WITH A FUNCTION BODY FOR THE HOLE MARKED '?'",
    "- DO NOT include the program sketch in your reply",
    "- DO NOT include a period at the end of your response and DO NOT use markdown",
  ];

  let advanced_reasoning_prompt = (completion_token: string) => [
    "CODE COMPLETION INSTRUCTIONS:",
    "- First, provide a brief discussion of your approach and reasoning",
    "- Then, provide your code completion for the hole marked '"
    ++ completion_token
    ++ "' enclosed in triple backticks",
    "- The program hole marked '"
    ++ completion_token
    ++ "' consists of the hole token '?a' appended with a series of characters to help you differentiate between other holes in the program",
    "- Your response MUST include two parts:",
    "  1. A discussion section explaining your approach",
    "  2. Your code completion inside triple backticks",
    "- DO NOT include anything else in your response",
    "- DO NOT provide multiple code suggestions",
    "- DO NOT include any text after the code block",
    "- Here is an example of the format you should follow:",
    "- Discussion:",
    "- The function takes an integer n as input and returns a float.",
    "- The base case returns 1.0 when n is 0, ensuring the function adheres to the expected Float return type.",
    "- For all other cases, the function returns 2.0, maintaining consistency in return type while providing a simple branching structure.",
    "  ```",
    "  fun n -> if n == 0 then 1.0 else 2.0",
    "  ```",
    "- The code completion should be a functional, idiomatic replacement for the program hole marked '"
    ++ completion_token
    ++ "' in the provided program sketch",
    // Uncomment either of the following lines to test error rounds
    // "- However, make sure that your initial response CAUSES A TYPE ERROR in the program. Then, fix it in your second response",
    // "- However, it is CRITICALLY important to make sure that your response ALWAYS CAUSES A TYPE ERROR in the program, no matter how many times you are re-prompted",
    "- Reply ONLY with a SINGLE replacement term for the unique distinguished hole marked '"
    ++ completion_token
    ++ "'",
    "- DO NOT suggest more replacements for other holes in the sketch (marked '?', '??', or '?a'), or implicit holes",
    "- This is critical, and I am going to reiterate it: DO NOT suggest more than one replacement term. It should ONLY be for the hole marked '"
    ++ completion_token
    ++ "'",
    "- For example, if you are being asked to complete 'let f = ? in "
    ++ completion_token
    ++ "', your response should ONLY be a single replacement term for the hole marked '"
    ++ completion_token
    ++ "', NOT a replacement term for the hole marked '?'",
    "- i.e. You should ONLY respond with a function application, or something else which would be a valid replacement term for the hole marked '"
    ++ completion_token
    ++ "'",
    "- If you wish to include a hole in your response, use '?a' only, without the appended characters that were used to identify the specific hole you were given",
    "- IT WOULD BE A HUGE MISTAKE TO RESPOND WITH A FUNCTION BODY FOR THE HOLE MARKED '?'",
    "- DO NOT include the program sketch in your reply",
    "- DO NOT include a period at the end of your response and DO NOT use markdown",
  ];

  let hazel_syntax_notes = [
    "HAZEL SYNTAX NOTES:",
    "- Hazel uses C-style function application syntax, with parenthesis around comma-separated arguments",
    "- Function application is ALWAYS written using parentheses and commas: use 'function(arg1, arg2)'. DO NOT just use spaces between the function name and arguments.",
    "- Function parameters are ALWAYS commas separated: 'fun arg1, arg2 -> <exp>'. DO NOT use spaces to separate function arguments.",
    "- There is no dot accessor notation for tuples; DO NOT use tuple.field. use pattern matching for destructuring: let (field, _) = tuple in ...",
    "- The following ARE NOT Hazel keywords. DO NOT use these keywords: switch, with, of, rec. ALWAYS omit these keywords",
    "- Pattern matching is ALWAYS written a 'case ... end' expression. Cases MUST END in an 'end' keyword. DO NOT USE any other keyword besides 'case' to do pattern matching.  DO NOT USE a 'with' or 'of' keyword with 'case', just start the list of rules. Pattern matching rules use syntax '| pattern => expression'. Note the '=>' arrow.",
    "- The ONLY way to define a named function is by using a function expression nested in a let expression like 'let <pat> = fun <pat> -> <exp> in <exp'. There is no support for specifying the function arguments directly as part of the let. DO NOT write function arguments in the let pattern.",
    "- No 'rec' keyword is necessary for 'let' to define a recursive function. DO NOT use the 'rec' keyword with 'let'.",
    "- Format the code with proper linebreaks",
  ];

  let task_completion_toolkit = [
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
    "    Example: ```goto_definition x``` selects 'let x = <definition> in'",
    /* File Editing Tools */
    "- FILE EDITING TOOLS:",
    "  * ```edit_code <code>``` - Replaces the currently selected definition with code.",
    /* Task Tools */
    "- TASK TOOLS:",
    "  * ```submit``` - Ends the iterative process and finalizes the task.",
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
  ];

  let few_shot_comp_examples = [
    "The following are several example dialogues and actions taken for example task completions.",
    "Also note that you are working in the Hazel programming language, thus pay attention to the syntax and semantics of the language: ",
    "Note that you can and should (when favorable) make multiple tool calls in a single response, though these examples only show a single tool call per response.",
    /* Example 1: Fix a type error in a simple function */
    "# Example 1: Fix a type error in a Hazel function #",
    "# Task: Find and fix the type error in the add function #",
    "# Current sketch: let add = fun x -> x + \"hello\" in add(5) #",
    "USER: Can you fix the type error in the add function?",
    "ASSISTANT: I'll fix the type error in the add function. First, I need to navigate to the function definition.",
    "```goto_definition add```",
    "SYSTEM: Selected: let add = fun x -> x + \"hello\" in",
    "ASSISTANT: I see the issue. The function is trying to add an integer and a string, which isn't valid in Hazel. I'll replace it with correct code that adds two numbers.",
    "```edit_code let add = fun x -> x + 5 in```",
    "SYSTEM: Updated and selected: let add = fun x -> x + 5 in",
    "ASSISTANT: Let's actually square the input instead.",
    "```edit_code let square = fun x -> x * x in```",
    "SYSTEM: Updated and selected: let square = fun x -> x * x in",
    "ASSISTANT: The type error is now fixed. The add function correctly squares its input. I'll submit the completed task.",
    "```submit```",
    /* Example 2: Modify a Boolean function */
    "# Example 2: Modify a Boolean function in Hazel #",
    "# Task: Update the 'negate' function to check if a number is less than 10 instead of negating a Boolean #",
    "# Current sketch: let negate: Bool -> Bool = fun b -> !b in negate(true)#",
    "USER: Please change the 'negate' function to check if a number is less than 10 instead.",
    "ASSISTANT: I'll modify the negate function to check if a number is less than 10. First, I need to find the function definition.",
    "```goto_definition negate```",
    "SYSTEM: Selected: let negate: Bool -> Bool = fun b -> !b in",
    "ASSISTANT: Now I'll replace the function with one that checks if a number is less than 10. I need to update both the type signature and implementation.",
    "```edit_code let negate: Int -> Bool = fun n -> n < 10 in```",
    "SYSTEM: Updated and selected: let negate: Int -> Bool = fun n -> n < 10 in",
    "ASSISTANT: The function has been updated to check if a number is less than 10. Note that the call site still has negate(true), which would need to be updated separately. I'll submit this task.",
    "```submit```",
    /* Example 3: Working with algebraic data types */
    "# Example 3: Fix a pattern matching function for an algebraic data type #",
    "# Task: Fix the 'is_empty' function to correctly check if a list is empty #",
    "# Current sketch: let is_empty: [Int] -> Bool = fun xs -> case xs | x::rest => false end in is_empty([]) #",
    "USER: The is_empty function doesn't handle empty lists correctly. Can you fix it?",
    "ASSISTANT: I'll fix the is_empty function to correctly handle empty lists. First, I need to find the function definition.",
    "```goto_definition is_empty```",
    "SYSTEM: Selected: let is_empty: [Int] -> Bool = fun xs -> case xs | x::rest => false end in",
    "ASSISTANT: I see the issue. The case expression only handles non-empty lists and returns false. It's missing a pattern for empty lists. I'll add the missing pattern.",
    "```edit_code let is_empty: [Int] -> Bool = fun xs -> case xs | [] => true | x::rest => false end in```",
    "SYSTEM: Updated and selected: let is_empty: [Int] -> Bool = fun xs -> case xs | [] => true | x::rest => false end in",
    "ASSISTANT: The is_empty function now correctly checks if a list is empty. It returns true for empty lists and false for non-empty lists. Task completed.",
    "```submit```",
    /* Example 4: Fix a type error in a recursive function */
    "# Example 4: Fix a type error in a recursive function #",
    "# Task: Fix the 'sum' function that's supposed to add all numbers in a list #",
    "# Current sketch: let sum = fun xs -> case xs | [] => 0 | h::t => h + sum(t) end in sum([1, 2, 3]) #",
    "USER: The sum function has a type error. Please add the missing type annotation.",
    "ASSISTANT: I'll fix the sum function by adding a type annotation. First, I need to look at its definition.",
    "```goto_definition sum```",
    "SYSTEM: Selected: let sum = fun xs -> case xs | [] => 0 | h::t => h + sum(t) end in",
    "ASSISTANT: I see the issue. The function is recursive but missing a type annotation, which is required for recursive functions in Hazel. I'll add the appropriate type annotation for a function that sums a list of integers.",
    "```edit_code let sum: [Int] -> Int = fun xs -> case xs | [] => 0 | h::t => h + sum(t) end in```",
    "SYSTEM: Updated and selected: let sum: [Int] -> Int = fun xs -> case xs | [] => 0 | h::t => h + sum(t) end in",
    "ASSISTANT: The sum function now has the correct type annotation [Int] -> Int, indicating it takes a list of integers and returns a single integer. This fixes the type error for the recursive function. Task completed.",
    "```submit```",
    /* Example 5: Fix a function with tuple pattern matching */
    "# Example 5: Fix a function with tuple pattern matching #",
    "# Task: Fix the 'add_pairs' function that's supposed to add corresponding elements in two lists #",
    "# Current sketch: let add_pairs = fun (xs, ys) -> case (xs, ys) | [], _ => [] | _, [] => [] | h1::t1, h2::t2 => h1 + h2 :: add_pairs(t1, t1) end in add_pairs([1, 2], [3, 4]) #",
    "USER: The add_pairs function has a bug where it passes the wrong argument to the recursive call. Can you fix it?",
    "ASSISTANT: I'll fix the bug in the add_pairs function. First, I need to find the function definition.",
    "```goto_definition add_pairs```",
    "SYSTEM: Selected: let add_pairs = fun (xs, ys) -> case (xs, ys) | [], _ => [] | _, [] => [] | h1::t1, h2::t2 => h1 + h2 :: add_pairs(t1, t1) end in",
    "ASSISTANT: I see the bug. In the recursive call add_pairs(t1, t1), the function is passing t1 twice instead of passing t1 and t2. I'll fix this by updating the recursive call to use the correct arguments.",
    "```edit_code let add_pairs = fun (xs, ys) -> case (xs, ys) | [], _ => [] | _, [] => [] | h1::t1, h2::t2 => h1 + h2 :: add_pairs(t1, t2) end in```",
    "SYSTEM: Updated and selected: let add_pairs = fun (xs, ys) -> case (xs, ys) | [], _ => [] | _, [] => [] | h1::t1, h2::t2 => h1 + h2 :: add_pairs(t1, t2) end in",
    "ASSISTANT: The bug in the add_pairs function is now fixed. It correctly passes t1 and t2 to the recursive call, which will properly process all elements in both lists. Task completed.",
    "```submit```",
    /* Example 6: Working with algebraic data types */
    "# Example 6: Improve an expression evaluator #",
    "# Task: Add support for multiplication to the 'eval' function for arithmetic expressions #",
    "# Current sketch: type Exp = +Num(Int) +Add(Exp, Exp) in let eval = fun e -> case e | Num(n) => n | Add(e1, e2) => eval(e1) + eval(e2) end in eval(Add(Num(2), Num(3))) #",
    "USER: Can you extend the Exp type and eval function to support multiplication?",
    "ASSISTANT: I'll extend the expression evaluator to support multiplication. First, I need to look at the current implementation.",
    "```goto_definition Exp```",
    "SYSTEM: Selected: type Exp = +Num(Int) +Add(Exp, Exp) in",
    "ASSISTANT: I'll modify the Exp type to include a constructor for multiplication operations.",
    "```edit_code type Exp = +Num(Int) +Add(Exp, Exp) +Mul(Exp, Exp) in```",
    "SYSTEM: Updated and selected: type Exp = +Num(Int) +Add(Exp, Exp) +Mul(Exp, Exp) in",
    "ASSISTANT: Now I need to update the eval function to handle the new Mul constructor.",
    "```goto_definition eval```",
    "SYSTEM: Selected: let eval = fun e -> case e | Num(n) => n | Add(e1, e2) => eval(e1) + eval(e2) end in",
    "ASSISTANT: I'll add a pattern for multiplication to the eval function.",
    "```edit_code let eval = fun e -> case e | Num(n) => n | Add(e1, e2) => eval(e1) + eval(e2) | Mul(e1, e2) => eval(e1) * eval(e2) end in```",
    "SYSTEM: Updated and selected: let eval = fun e -> case e | Num(n) => n | Add(e1, e2) => eval(e1) + eval(e2) | Mul(e1, e2) => eval(e1) * eval(e2) end in",
    "ASSISTANT: The expression evaluator now supports multiplication! I've added a Mul constructor to the Exp type and updated the eval function to handle multiplication expressions by recursively evaluating both sub-expressions and multiplying the results. Task completed.",
    "```submit```",
  ];

  let mk_suggestion_prompt =
      (
        {instructions, syntax_notes, _}: Options.t,
        completion_token: string,
        advanced_reasoning: bool,
      )
      : string =>
    String.concat(
      "\n",
      (
        instructions
          ? prelude
            @ (
              advanced_reasoning
                ? advanced_reasoning_prompt(completion_token)
                : main_prompt(completion_token)
            )
          : []
      )
      @ (syntax_notes ? hazel_syntax_notes : []),
    );
};

module Completion = {
  let get_static_context =
      (
        expected_type: bool,
        relevant_ctx: bool,
        ci: Info.t,
        hole_label: string,
      )
      : list(string) =>
    switch (ci) {
    | InfoExp({ana, ctx, _})
    | InfoPat({ana, ctx, _}) =>
      let expected = RelevantTypes.get(ctx, ana, hole_label);
      let relevant = RelevantValues.get(ctx, ana);
      (expected_type ? ["expected_ty: " ++ expected] : [])
      @ (relevant_ctx ? ["relevant_ctx:\n " ++ relevant] : []);
    | InfoTyp(_)
    | InfoTPat(_)
    | Secondary(_) => []
    };

  let prompt =
      (
        options: Options.t,
        ci: Info.t,
        sketch: Segment.t,
        hole_label: string,
        advanced_reasoning: bool,
      )
      : OpenRouter.prompt =>
    [
      OpenRouter.mk_system_msg(
        SystemPrompt.mk_suggestion_prompt(
          options,
          hole_label,
          advanced_reasoning,
        ),
      ),
    ]
    @ CompletionExamples.get(
        options.num_examples,
        hole_label,
        advanced_reasoning,
      )
    @ [
      OpenRouter.mk_user_msg(
        String.concat(
          "\n",
          ["sketch: " ++ ErrorPrint.Print.seg(~holes=Some("?"), sketch)]
          @ get_static_context(
              options.expected_type,
              options.relevant_ctx,
              ci,
              hole_label,
            ),
        ),
      ),
    ];
};

module Composition = {
  let get_static_context = (relevant_ctx: bool, ci: Info.t): list(string) =>
    switch (ci) {
    | InfoExp({ana, ctx, _})
    | InfoPat({ana, ctx, _}) =>
      let relevant = RelevantValues.get(ctx, ana);
      relevant_ctx ? ["relevant_ctx:\n " ++ relevant] : [];
    | InfoTyp(_)
    | InfoTPat(_)
    | Secondary(_) => []
    };

  // Finds the first matching variable as 'name' in the context
  // highlights the variable and definition (excluding the body)
  let goto_definition =
      (name: string, editor: CodeWithStatics.Model.t): list(Action.t) => {
    let statics = CodeWithStatics.Model.get_statics(editor);
    // Find the first matching variable in the context using fold
    // TODO: Handle shadowed variables
    let matching_id =
      Id.Map.fold(
        (_, info, acc) => {
          switch (acc) {
          | Some(_) => acc // Already found a match
          | None =>
            let ctx = Info.ctx_of(info);
            switch (Ctx.lookup_var(ctx, name)) {
            | Some(entry) => Some(entry.id)
            | None => None
            };
          }
        },
        statics.info_map,
        None,
      );
    // Return appropriate action based on whether we found a match
    switch (matching_id) {
    | Some(id) => [
        Action.Jump(TileId(id)),
        Action.Select(Smart(3)),
        Action.Copy,
      ]
    | None => [Action.Select(Term(Id(Id.invalid, Direction.Left)))]
    };
  };

  let edit = (code: string): list(Action.t) => {
    [
      // TODO: Might be helpful to paste a segment instead of a string
      // This may allow for better error handling.
      // We paste the code edit, then reselect the definition, and copy
      // to clipboard shim to give context to assistant.
      Action.Paste(String(code)),
      Action.Select(Smart(3)),
      Action.Copy,
    ];
  };

  let mk_prompt =
      (options: Options.t, ci: Info.t, sketch: Segment.t, init: bool): string => {
    let prelude_and_toolkit =
      String.concat("\n", SystemPrompt.task_completion_toolkit);
    let few_shot_examples =
      String.concat("\n", SystemPrompt.few_shot_comp_examples);
    String.concat(
      "\n",
      (init ? [prelude_and_toolkit] : [])
      @ [
        "Current sketch: " ++ ErrorPrint.Print.seg(~holes=Some("?"), sketch),
      ],
      //@ get_static_context(options.relevant_ctx, ci),
    );
  };
};

module ErrorRound = {
  open OptUtil.Syntax;
  module StringSet = Set.Make(String);

  let get_parse_errs =
      (sketch_z: Zipper.t, completion: string): Result.t(Zipper.t, string) =>
    //NOTE: This function is pretty basic; reporting approach could be improved
    switch (
      {
        let* sketch_z = Destruct.go(Left, sketch_z);
        let* sketch_z = Destruct.go(Left, sketch_z);
        Perform.paste(sketch_z, completion);
      }
    ) {
    | None => Error("Undocumented parse error, no feedback available")
    | Some(completion_z) =>
      switch (
        completion_z.backpack
        |> List.map((s: Selection.t) =>
             Printer.of_segment(~holes=None, s.content)
           )
      ) {
      | [_, ..._] as orphans =>
        Error(
          "The parser has detected the following unmatched delimiters:. The presence of a '=>' in the list likely indicates that a '->' was mistakingly used in a case expression: "
          ++ String.concat(", ", orphans),
        )
      | [] => Ok(completion_z)
      }
    };

  let statics_of_exp_zipper =
      (init_ctx: Ctx.t, z: Zipper.t): (Info.exp, Statics.Map.t) =>
    Statics.uexp_to_info_map(
      ~ctx=init_ctx,
      ~ancestors=[],
      MakeTerm.from_zip_for_sem(z).term,
      Id.Map.empty,
      ~duplicates=[],
      ~expected_labels=None,
      ~label_sort=false,
    );

  let mk_report = (ctx: Ctx.t, z: Zipper.t, reply: string): ErrorPrint.t =>
    // TODO: Currently this only works in expression position
    switch (get_parse_errs(z, reply)) {
    | Error(err) => ParseError(err)
    | Ok(full_z) =>
      let (_, info_map) = statics_of_exp_zipper(ctx, z);
      let static_errs_sketch = ErrorPrint.all(info_map);
      let (_, info_map) = statics_of_exp_zipper(ctx, full_z);
      let static_errs_full = ErrorPrint.all(info_map);
      if (List.length(static_errs_full) == 0) {
        NoErrors;
      } else {
        let sketch_errs = StringSet.of_list(static_errs_sketch);
        let new_errs =
          List.filter(
            err => !StringSet.mem(err, sketch_errs),
            static_errs_full,
          );
        if (List.length(new_errs) == 0) {
          NoErrors;
        } else {
          StaticErrors(new_errs);
        };
      };
    };

  let mk_reply =
      (ci: Info.t, sketch_z: Zipper.t, reply: string): option(string) => {
    // TODO: Currently this only works in expression position
    let wrap = (intro, errs) =>
      [intro]
      @ errs
      @ [
        "Please try to address the error(s) by updating your previous code suggestion",
        "Please respond ONLY with the update suggestion",
      ]
      |> String.concat("\n");
    let error_report = mk_report(Info.ctx_of(ci), sketch_z, reply);
    switch (error_report) {
    | NoErrors => None
    | ParseError(err) =>
      Some(wrap("The following parse error occured:", [err]))
    | StaticErrors(errs) =>
      Some(wrap("The following static errors were discovered:", errs))
    };
  };
};
