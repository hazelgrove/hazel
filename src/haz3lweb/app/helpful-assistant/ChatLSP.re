open Util;
open Haz3lcore.Info;
open Haz3lcore;

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
    "    Example: ```goto_definition x``` selects 'let x = 1 in' in the program 'let x = 1 in x + 1'",
    "  * ```goto_body <variable_name>``` - Selects the body of the variable's let binding.",
    "    After using this, any file editing actions will target the body of the selected definition.",
    "    This is particularly useful when needing to update the contents of the final let expression in a program path/scope (eg. function, if, etc).",
    "    Example: ```goto_body x``` selects 'x + 1' in the program 'let x = 1 in x + 1'",
    /* File Editing Tools */
    "- FILE EDITING TOOLS:",
    "  * ```edit <code>``` - Replaces the currently selected definition with code.",
    "  * ```insert_before <code>``` - Inserts code before the currently selected definition.",
    "  * ```insert_after <code>``` - Inserts code after the currently selected definition.",
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

  let few_shot_comp_examples = [
    /* Example 1: Fixing a type error in a function */
    "Example 1: Fixing a type error in a function",
    "PROGRAM SKETCH:
let add = fun x -> x + \"hello\" in  # Type error: adding Int and String
add(5)  # Error occurs when trying to add 5 and \"hello\"
",
    "USER: Fix the type error in the add function.",
    "ASSISTANT: I'll fix the type error in the add function by first locating it and then making the necessary changes.

```goto_definition add```

Now I'll fix the type error by making sure both operands have compatible types:

```edit let add = fun x -> x + 5 in```

Let me see how those changes affect the program.",
    "USER: SYSTEM: after your most recent edits, here is the current state of the code.",
    "ASSISTANT: The function now correctly adds an integer to the input. Task completed:

```submit```",
    /* Example 2: Creating a list reversal function */
    "Example 2: Creating a list reversal function",
    "PROGRAM SKETCH:
# The program has various list functions but is missing a reverse function
let map: forall a -> forall b -> (a -> b) -> [a] -> [b] = ... in
let filter: forall a -> (a -> Bool) -> [a] -> [a] = ... in
# Our task is to implement the missing reverse function
let reverse = ? in
[1, 2, 3]  # We want to be able to reverse this to [3, 2, 1]
",
    "USER: Create a reverse function for lists.",
    "ASSISTANT: I'll create a list reversal function that works on any type of list.

```goto_definition reverse```

I'll implement the reverse function with proper type annotations and recursion:

```edit let reverse: forall a -> [a] -> [a] =
  typfun a -> fun xs: [a] ->
    let rev_helper: ([a], [a]) -> [a] =
      fun (acc, rest) ->
        case rest
        | [] => acc
        | hd::tl => rev_helper(hd::acc, tl)
        end
    in
    rev_helper([], xs)
in```
",
    "USER: SYSTEM: after your most recent edits, here is the current state of the code.",
    "ASSISTANT: The function is complete and typed correctly:

```submit```",
    /* Example 3: Fixing multiple functions */
    "Example 3: Fixing multiple functions",
    "PROGRAM SKETCH:
# The program has mutually recursive even/odd functions with bugs
let (even: Int -> Bool, odd: Int -> Bool) =
(fun n -> if n == 0 then true else odd(n + 1),  # Bug: should be n - 1
 fun n -> if n == 1 then false else even(n + 1))  # Bug: base case should be n == 0
in
even(4)  # Should return true but will cause infinite recursion
",
    "USER: Fix both the even and odd functions.",
    "ASSISTANT: I'll fix both recursive functions by first examining the even function:

```goto_definition even```

Now I'll correct the base case and recursive call:

```edit let even: Int -> Bool =
  fun n ->
    if n == 0
    then true
    else odd(n - 1)
in```

Now I'll move to the odd function:

```goto_definition odd```

And fix its implementation:

```edit let odd: Int -> Bool =
  fun n ->
    if n == 0
    then false
    else even(n - 1)
in```
",
    "USER: SYSTEM: After your most recent edits, here is the current state of the code.",
    "ASSISTANT: Both functions are now correctly implemented with proper mutual recursion:

```submit```",
    /* Example 4: Updating an ADT and related function */
    "Example 4: Updating an ADT and related function",
    "PROGRAM SKETCH:
# The program has a simple expression language and evaluator
type Exp =
  + Var(String)
  + Lam(String, Exp)
  + Ap(Exp, Exp)
in

type Result =
  + Error(String)
  + Ok(Exp)
in

let eval: Exp -> Result =
  fun e ->
    case e
    | Var(n) => Error(\"Free Variable\")
    | Lam(x, body) => Ok(Lam(x, body))
    | Ap(e1, e2) =>
        case eval(e1)
        | Ok(Lam(x, body)) =>
            case eval(e2)
            | Error(err) => Error(err)
            | Ok(arg) => eval(subst(arg, x, body))
            end
        | _ => Error(\"Not a Function\")
        end
    end
in
",
    "USER: Add a Mult constructor to the Exp type and update the eval function to handle it.",
    "ASSISTANT: I'll first check the Exp type definition:

```goto_definition Exp```

Now I'll update it to include the Mult constructor:

```edit type Exp =
  + Var(String)
  + Lam(String, Exp)
  + Ap(Exp, Exp)
  + Mult(Exp, Exp)
in```

Next, I'll update the eval function to handle the new constructor:

```goto_definition eval```

```edit let eval: Exp -> Result =
  fun e ->
    case e
    | Var(n) => Error(\"Free Variable\")
    | Lam(x, body) => Ok(Lam(x, body))
    | Ap(e1, e2) =>
        case eval(e1)
        | Ok(Lam(x, body)) =>
            case eval(e2)
            | Error(err) => Error(err)
            | Ok(arg) => eval(subst(arg, x, body))
            end
        | _ => Error(\"Not a Function\")
        end
    | Mult(e1, e2) =>
        case eval(e1), eval(e2)
        | Ok(Lam(_, _)), _ => Error(\"Expected number, got function\")
        | _, Ok(Lam(_, _)) => Error(\"Expected number, got function\")
        | Error(err), _ => Error(err)
        | _, Error(err) => Error(err)
        | Ok(Var(n1)), Ok(Var(n2)) =>
            if is_number(n1) && is_number(n2)
            then Ok(Var(string_of_int(int_of_string(n1) * int_of_string(n2))))
            else Error(\"Can only multiply numbers\")
        end
in```
",
    "USER: SYSTEM: after your most recent edits, here is the current state of the code.",
    "ASSISTANT: Both the type and evaluation function have been updated properly:

```submit```",
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
  type loc_of_edit =
    | Before
    | After
    | Current;

  type loc_of_goto =
    | Body
    | Definition;

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
  let goto =
      (editor: CodeWithStatics.Model.t, name: string, loc: loc_of_goto)
      : list(Action.t) => {
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
        // Moving left by token is essentially a hacky method to get
        // off of a variable name (term), and triple/quad click on let binding
        // itself (this properly highlights full variable name and
        // definition when type annotation exists)
        Action.Move(Local(Left(ByToken))),
        switch (loc) {
        | Definition => Action.Select(Smart(3))
        | Body => Action.Select(Smart(4))
        },
        Action.Copy,
      ]
    | None => [Action.Select(Term(Id(Id.invalid, Direction.Left)))]
    };
  };

  let edit = (code: string, loc: loc_of_edit): list(Action.t) => {
    // TODO: Might be helpful to paste a segment instead of a string
    // This may allow for better error handling.
    switch (loc) {
    | Before => [
        Action.Unselect(Some(Left)), // Unselect current definition
        Action.Paste(String(code ++ "\n")), // Paste new code
        Action.Select(Smart(3)), // Select the pasted code
        Action.Copy // Copy the pasted code
      ]
    | After => [
        Action.Unselect(Some(Direction.Right)), // Unselect current definition
        Action.Paste(String("\n" ++ code)), // Paste new code
        Action.Select(Smart(3)), // Select the pasted code
        Action.Copy // Copy the pasted code
      ]
    | Current =>
      String.length(code) == 0
        ? [
          Action.Paste(String(code)), // Replace current definition
          Action.Destruct(Left),
          Action.Select(Smart(3)), // Select the pasted code
          Action.Copy // Copy the pasted code
        ]
        : [
          Action.Paste(String(code)), // Replace current definition
          Action.Select(Smart(3)), // Select the pasted code
          Action.Copy // Copy the pasted code
        ]
    // We paste the code edit, then reselect the definition, and copy
    // to clipboard shim to give context to assistant.
    };
  };

  let mk_prompt =
      (options: Options.t, ci: Info.t, sketch: Segment.t, init: bool): string => {
    let (_, _) = (options, ci); // TODO: Either remove params or update function to use params
    let prelude_and_toolkit =
      String.concat("\n", SystemPrompt.task_completion_toolkit);
    let few_shot_examples =
      String.concat("\n", SystemPrompt.few_shot_comp_examples);
    let hazel_syntax_notes =
      String.concat("\n", SystemPrompt.hazel_syntax_notes);
    String.concat(
      "\n",
      (
        init
          ? [prelude_and_toolkit, hazel_syntax_notes, few_shot_examples] : []
      )
      @ [
        "PROGRAM SKETCH: " ++ ErrorPrint.Print.seg(~holes=Some("?"), sketch),
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
