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
    "- The user will provide you with a task to complete.",
    "- You will need to complete the task using the toolkit provided.",
    "- The toolkit is a list of tool calls which you can make to complete the task.",
    "- You will need to use ONLY tool calls from the toolkit to complete the task.",
    "- These act as a sort of interface to interact with the code base and language server.",
    "- Critically, they are structure-based, meaning they interact purely with the high-level, definition-based structure of the code base.",
    "- There are 3 categories of tool calls: 'file viewing', 'file editing', and 'task'.",
    "- 'file viewing' allows you to navigate through out a code editor.",
    "- 'file editing' allows you to make changes to the code base.",
    "- 'task' is trivial, and has only one command—submit/end the iterative tool call process.",
    "- You MUST use the correct tool call for the action you need to take.",
    "- You MUST use the correct argument(s) for the tool call.",
    "- You MUST use the correct format for the tool call.",
    "- Here are the 'file viewing' tool calls:",
    "- 'goto_definition <variable_name>' Selects the variable let binding and definition at the nearest definition of the matched variable_name.",
    "- eg. if variable_name is 'x', and a definition for 'x' exists, then the selection will be 'let x = <definition> in'. After going to a variable name,",
    "- you can assume any file editing actions will be performed on the selected let binding and definition. eg. deleting would basically to give replacement text as",
    "- the empty string ' '.",
    //"- 'goto_type_definition <variable_name>' Places the cursor at the nearest type definition for the matched variable_name.",
    //"- 'show_references <variable_name>' displays all defintions where the variable matching variable_name is referenced.",
    //"- 'scroll_up' moves the cursor to the preceding definition.",
    //"- 'scroll_down' moves the cursor to the succeeding definition.",
    "- Here are the 'file editing' tool calls:",
    "- 'replace <replacement_text>' Replaces the definition the cursor is currently at with replacement_text.",
    //"- 'insert <new_text>' Inserts new_text directly after the definition the cursor is currently at.",
    //"- 'delete' Deletes the definition the cursor is currently at.",
    "- Here are the 'task' tool calls:",
    "- 'submit' ends the iterative tool call process, finalizing the task.",
    "- That is all the tools you need. Notice that all of them are structure-based, meaning they interact purely with the high-level, definition-based structure of the code base.",
    "- The 'cursor' really represents an entire definition which you are currently at.",
    "- You can think of this more so as having the entire variable and definition of a let binding selected/highlighted.",
    "- You are highly encourages to reason and chain of thought before making a tool call.",
    "- Because of this, we will need to parse the tool call out of your response.",
    "- You MUST respond with the tool call in the following format: ```tool_call <required_argument>```",
    "- For example, if you need to make a 'goto_definition' tool call, your response should be: ```goto_definition foo```",
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
  let goto_variable =
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
    | Some(id) => [Action.Jump(TileId(id)), Action.Select(Smart(3))]
    | None => [Action.Select(Term(Id(Id.invalid, Direction.Left)))]
    };
  };

  let replace = (replacement_text: string): list(Action.t) => {
    [
      // TODO: Might be helpful to paste a segment instead of a string
      // This may allow for better error handling.
      Action.Paste(String(replacement_text)),
    ];
  };

  let insert =
      (text: string, editor: CodeWithStatics.Model.t): list(Action.t) => {
    [Action.Paste(String(text))];
  };

  let prompt =
      (options: Options.t, ci: Info.t, sketch: Segment.t)
      : list(OpenRouter.message) => {
    let prelude_and_toolkit =
      String.concat("\n", SystemPrompt.task_completion_toolkit);
    let prelude_and_toolkit = OpenRouter.mk_system_msg(prelude_and_toolkit);
    [prelude_and_toolkit]
    @ [
      OpenRouter.mk_user_msg(
        String.concat(
          "\n",
          ["sketch: " ++ ErrorPrint.Print.seg(~holes=Some("?"), sketch)]
          @ get_static_context(options.relevant_ctx, ci),
        ),
      ),
    ];
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
