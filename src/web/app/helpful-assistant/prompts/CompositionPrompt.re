open Util;

let hazel_syntax_notes = HazelSyntaxNotes.self;

let summarized_hazel_docs = SummarizedHazelDocs.self;

let instructions = [
  "<instructions>",
  "You are an expert AI programming agent operating in the Hazel programming language.",
  "You are working with a user to accomplish a programming task in a paired programming setting.",
  "The user will ask you a question or to perform a task (implement a feature, fix an issue, etc).",
  "You are a professional coding agent, meaning it is your duty to complete the user's task or attempt to complete their task until you decide",
  "the task is complete or it is absolutely infeasible to complete.",
  "To reiterate, you are operating in the Hazel programming language. This is a known to be a low-resource language,",
  "meaning you will be provided with relevant syntax and semantic information about the programming language",
  "that you can carefully study and review when generating your responses.",
  "NEVER try to write code from another programming language other than Hazel.",
  "You may explain and reason about the program/task/user query, but aim to keep your thinking and explanations concise and to the point.",
  "If the user wants you to implement a feature that is quite complex, you should break it down into smaller tasks to work through step by step.",
  "You do not need to repeat code in your response. You can simply call the tool to insert the code.",
  "After calling a tool, you should pick up immediately from where you left off—No need to repeat or summarize what you've been doing.",
  "You should avoid explicitly mentioning tool calls to the user. Your conversation with the user should be natural, as if you were their human pair programming partner.",
  "We will now provide you with the following:\n",
  "1. A toolkit along with a specification on how to call these tools throughout the attempted completion of the task.\n",
  "2. Hazel syntax notes.\n",
  "3. A brief summary of Hazel documentation.\n",
  "4. A series of GOLDEN EXAMPLES from agents who successfully implemented user-requested features.\n",
  "You should frequently come back and reference each of the toolkit, syntax notes, documentation, and golden standard examples.",
  "Keep your chats brief and concise, briefly communicating with the user your plan-of-action.",
  "After making a tool call, pick up immediately from where you left off.",
  "That is, do not repeat yourself or try to summarize what you've been doing.",
  "You should use markdown to format your text responses, in a way such that the user can easily read and understand your thinking, intentions, and plan-of-action.",
  "Available markdown features include:\n",
  "1. bold\n",
  "2. italic\n",
  "3. inline code\n",
  "4. headers\n",
  "5. blockquote\n",
  "6. thematic break\n",
  "7. lists\n",
  "8. links\n",
  "</instructions>",
];

let toolkit = [
  "<toolkitInstructions>",
  "You are to complete user-specified tasks using only the tools provided.",
  "This toolkit contains specific action commands to navigate the sketch and modify code,",
  "essentially giving you a sort of cursor to work with and control.",
  "All actions commands interact with the high-level, definition-based structure of the program.",
  "In a sense, these allow you to navigate and alter meaningful semantic chunks of the program, akin to a structure editor (but with higher-level control).",
  "</toolkitInstructions>",
  "<toolkitNotes>",
  "You are an LLM placed in an environment where you are equipped with TOOLS.",
  "Every tool call will perform an action on the structure of the program and give you updated feedback on the current sketch, any errors present, and your currently selected code.",
  "A strong recommendation is to break a complex task into smaller, more manageable steps,",
  "where once broken into smaller steps, you can implement each step in as few responses as possible.",
  "If you do NOT make a tool call in your response, you are effectively submitting the task to the user.",
  "You need NOT make a tool call if the user asks a question that does not require any editing of their code.",
  "</toolkitNotes>",
];

// IDEA: Give the agent a modified version of the program, where each variable is guaranteed to be unique.
// This mitigates the possibility of unreachable shadowed variables.
// We should emphasize this to the agent, and make sure it omits the '_i' suffix from the variable names in any modifications it might make.
// We do this to EACH variable (even if they aren't shadowed), guaranteeing consistency.
// 1. Snapshot of the sketch
// 2. Append a unique suffix to each variable name
// 3. Send uniquely modified program to the agent
// 4. Agent should respond with variable_name = [unique_name] for appropriate tool calls
// 5. Receive agent's response, parse, and apply the changes to the ORIGINAL program

// idea: allow for multiple variables to be selected at once
//       or rather, allow for beginning and end of selection to be specified (based on variable definitons)
// let goto_definition: API.Json.t =
//   `Assoc([
//     ("type", `String("function")),
//     (
//       "function",
//       `Assoc([
//         ("name", `String("goto_definition")),
//         (
//           "description",
//           `String(
//             "Selects the definition of the given variable name. Eg. goto_definition x will select ```let x = 1 in``` given a program ```let y = 0 in\nlet x = 1 in\nx + y```.",
//           ),
//         ),
//         (
//           "parameters",
//           `Assoc([
//             ("type", `String("object")),
//             (
//               "properties",
//               `Assoc([
//                 (
//                   "variable",
//                   `Assoc([
//                     ("type", `String("string")),
//                     (
//                       "description",
//                       `String(
//                         "The name of the variable whose definition associated with its let binding is to be selected.",
//                       ),
//                     ),
//                   ]),
//                 ),
//               ]),
//             ),
//             ("required", `List([`String("variable")])),
//           ]),
//         ),
//       ]),
//     ),
//   ]);

// let goto_body: API.Json.t =
//   `Assoc([
//     ("type", `String("function")),
//     (
//       "function",
//       `Assoc([
//         ("name", `String("goto_body")),
//         (
//           "description",
//           `String(
//             "Selects the body of the given variable name. Eg. goto_body x will select ```x + y``` given a program ```let y = 0 in\nlet x = 1 in\nx + y```.",
//           ),
//         ),
//         (
//           "parameters",
//           `Assoc([
//             ("type", `String("object")),
//             (
//               "properties",
//               `Assoc([
//                 (
//                   "variable",
//                   `Assoc([
//                     ("type", `String("string")),
//                     (
//                       "description",
//                       `String(
//                         "The name of the variable whose body associated with its let bindingis to be selected.",
//                       ),
//                     ),
//                   ]),
//                 ),
//               ]),
//             ),
//             ("required", `List([`String("variable")])),
//           ]),
//         ),
//       ]),
//     ),
//   ]);

let update_pattern: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("update_pattern")),
        (
          "description",
          `String(
            "Updates the pattern (or typed pattern). The \"pattern\" is all of the"
            ++ "tokens enclosed between the \"let\" and \"=\" delimiters, exclusive. "
            ++ "Eg. Given a program ```let (x, y) : (Int, Int) = (1, 2) in...```) "
            ++ ", calling this tool with variable_name = x and new_pattern = \"(a, b) : (Int, Int)\" "
            ++ "will update the program to ```let (a, b) : (Int, Int) = (1, 2) in...```.",
          ),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "variable_name",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String("The name of the variable to be updated."),
                    ),
                  ]),
                ),
                (
                  "new_pattern",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String("The new pattern of the variable."),
                    ),
                  ]),
                ),
              ]),
            ),
            (
              "required",
              `List([`String("variable_name"), `String("new_pattern")]),
            ),
          ]),
        ),
      ]),
    ),
  ]);

let update_definition: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("update_definition")),
        (
          "description",
          `String(
            "Updates the definition of the given variable name. The \"definition\" is all of the"
            ++ "tokens enclosed between the \"=\" and \"in\" delimiters, exclusive. "
            ++ "Eg. Given a program ```let y = 0 in\nlet x = 1 in\nx + y```, "
            ++ "calling this tool with variable_name = x and new_definition = 3 "
            ++ "will update the program to ```let y = 0 in\nlet x = 3 in\nx + y```.",
          ),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "variable_name",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The name of the variable to have its definition updated.",
                      ),
                    ),
                  ]),
                ),
                (
                  "new_definition",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String("The new definition of the variable."),
                    ),
                  ]),
                ),
              ]),
            ),
            (
              "required",
              `List([`String("variable_name"), `String("new_definition")]),
            ),
          ]),
        ),
      ]),
    ),
  ]);

let update_body: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("update_body")),
        (
          "description",
          `String(
            "Updates the body of the given variable name. "
            ++ "The \"body\" is all of the tokens AFTER the \"in\" delimiter, exclusive. "
            ++ "Eg. Given a program ```let y = 0 in\nlet x = 1 in\nx + y```, "
            ++ "calling this tool with variable_name = x and new_body = (x * x) + (y * y) "
            ++ "will update the program to ```let y = 0 in\nlet x = 1 in\n(x * x) + (y * y)```.",
          ),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "variable_name",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The name of the variable to have its body updated.",
                      ),
                    ),
                  ]),
                ),
                (
                  "new_body",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String("The new body of the variable."),
                    ),
                  ]),
                ),
              ]),
            ),
            (
              "required",
              `List([`String("variable_name"), `String("new_body")]),
            ),
          ]),
        ),
      ]),
    ),
  ]);

let update_binding: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("update_binding")),
        (
          "description",
          `String(
            "Updates the ENTIRE binding associated with the given variable name. "
            ++ "The \"binding\" is all of the tokens between the \"let\" or \"type\" delimiter and the \"in\" delimiter, inclusive (meaning the \"let\" or \"type\" and \"in\" delimiters are included). "
            ++ "Eg. Given a program ```let y = 0 in\nlet x : Int = 1 in\nx + y```, "
            ++ "calling this tool with variable_name = x and new_binding = \"let b : Int = 0 in\" "
            ++ "will update the program to ```let y = 0 in\nlet b : Int = 0 in\nx + y```.",
          ),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "variable_name",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The name of the variable to have its binding updated.",
                      ),
                    ),
                  ]),
                ),
                (
                  "new_binding",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String("The new binding of the variable."),
                    ),
                  ]),
                ),
              ]),
            ),
            (
              "required",
              `List([`String("variable_name"), `String("new_binding")]),
            ),
          ]),
        ),
      ]),
    ),
  ]);

let delete_body: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("delete_body")),
        (
          "description",
          `String(
            "Deletes the body of the given variable name. "
            ++ "The \"body\" is all of the tokens AFTER the \"in\" delimiter, exclusive. "
            ++ "Eg. Given a program ```let y = 0 in\nlet x = 1 in\nx + y```, "
            ++ "calling this tool with variable_name = x "
            ++ "will update the program to ```let y = 0 in\nlet x = 1 in```. "
            ++ "(Note that your might action now might be to update the definition of y to take care of the unbounded variable x.)",
          ),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "variable_name",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The name of the variable to have its body deleted.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([])),
          ]),
        ),
      ]),
    ),
  ]);

let delete_binding: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("delete_binding")),
        (
          "description",
          `String(
            "Deletes the binding which binds the given variable name. "
            ++ "The \"binding\" is all of the tokens between the \"let\" or \"type\" delimiter and the \"in\" delimiter, inclusive (meaning the \"let\" or \"type\" and \"in\" delimiters are included). "
            ++ "Eg. Given a program ```let y = 0 in\nlet x : Int = 1 in\nx + y```, "
            ++ "calling this tool with variable_name = x "
            ++ "will update the program to ```let y = 0 in\nx + y```. "
            ++ "(Note that your might action now might be to update the body of y to take care of the unbounded variable x.)",
          ),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "variable_name",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The name of the variable to have its binding deleted.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("variable_name")])),
          ]),
        ),
      ]),
    ),
  ]);

let add_before: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("add_before")),
        (
          "description",
          `String(
            "Adds the given code before the binding of the given variable name, "
            ++ "i.e. puts code IMMEDIATELY before the \"let\" or \"type\" delimiter associated with the given variable name "
            ++ "Eg. Given a program ```let x = 0 in\nlet z = 1 in\nx + z```, "
            ++ "calling this tool with variable_name = z and code = \"let y = 3 in\\n\" "
            ++ "will update the program to ```let x = 0 in\nlet y = 3 in\nlet z = 1 in\nx + y```. "
            ++ "SPECIAL CASE: If no variable name is provided, the code is added to the BEGINNING of the program, which may be useful for certain tasks such as adding global variables.",
          ),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "variable_name",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The name of the variable to have code added before its binding.",
                      ),
                    ),
                  ]),
                ),
                (
                  "code",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The code to add before the binding of the variable.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("code")])),
          ]),
        ),
      ]),
    ),
  ]);

let add_after: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("add_after")),
        (
          "description",
          `String(
            "Adds the given code after the binding of the given variable name, "
            ++ "i.e. puts code IMMEDIATELY after the \"in\" delimiter associated with the given variable name "
            ++ "Eg. Given a program ```let x = 0 in\nlet y = 1 in\nx + y```, "
            ++ "calling this tool with variable_name = y and code = \"let z = 3 in\\n\" "
            ++ "will update the program to ```let x = 0 in\nlet y = 1 in\nlet z = 3 in\nx + y```. "
            ++ "SPECIAL CASE: If no variable name is provided, the code is added to the END of the program, which may be useful for certain tasks such as initializing empty sketches.",
          ),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "variable_name",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The name of the variable to have code added after its binding.",
                      ),
                    ),
                  ]),
                ),
                (
                  "code",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The code to add after the binding of the variable.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("code")])),
          ]),
        ),
      ]),
    ),
  ]);

let move_cursor: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("move_cursor")),
        (
          "description",
          `String(
            "Moves the cursor to the given variable name in the program. "
            ++ "Note that we have supplied you with a slightly modified version of the program, where each variable is guaranteed to be unique. "
            ++ "Thus mitigating the possibility of unreachable shadowed variables. "
            ++ "Because many of the tool calls depend on "
            ++ "This action is purely navigational, and has no effects on the state of the program itself. ",
          ),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "variable_name",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The name of the variable to move the cursor to.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("variable_name")])),
          ]),
        ),
      ]),
    ),
  ]);

/*
 let goto_type_definition: API.Json.t =
   `Assoc([
     ("type", `String("function")),
     (
       "function",
       `Assoc([
         ("name", `String("goto_type_definition")),
         (
           "description",
           `String(
             {|Selects the definition of the given type name. Eg. goto_type_definition t will select ```type t =
   + A(Bool)
   + B(Int, Bool)
 in``` given a program ```type t =
   + A(Bool)
   + B(Int, Bool)
 in
 let f = fun x: t ->
   case x
     | A(_) => "Argument has constructor A"
     | B(_) => "Argument has constructor B"
   end
 in f(A(false))```.|},
           ),
         ),
         (
           "parameters",
           `Assoc([
             ("type", `String("object")),
             (
               "properties",
               `Assoc([
                 (
                   "variable",
                   `Assoc([
                     ("type", `String("string")),
                     (
                       "description",
                       `String(
                         "The name of the variable whose definition associated with its let binding is to be selected.",
                       ),
                     ),
                   ]),
                 ),
               ]),
             ),
             ("required", `List([`String("variable")])),
           ]),
         ),
       ]),
     ),
   ]);

 let goto_type_body: API.Json.t =
   `Assoc([
     ("type", `String("function")),
     (
       "function",
       `Assoc([
         ("name", `String("goto_type_body")),
         (
           "description",
           `String(
             {|Selects the body of the given type name. Eg. goto_type_body t will select ```let f = fun x: t ->
   case x
     | A(_) => "Argument has constructor A"
     | B(_) => "Argument has constructor B"
   end
 in f(A(false))``` given a program ```type t =
   + A(Bool)
   + B(Int, Bool)
 in
 let f = fun x: t ->
   case x
     | A(_) => "Argument has constructor A"
     | B(_) => "Argument has constructor B"
   end
 in f(A(false))```.|},
           ),
         ),
         (
           "parameters",
           `Assoc([
             ("type", `String("object")),
             (
               "properties",
               `Assoc([
                 (
                   "variable",
                   `Assoc([
                     ("type", `String("string")),
                     (
                       "description",
                       `String(
                         "The name of the variable whose body associated with its let bindingis to be selected.",
                       ),
                     ),
                   ]),
                 ),
               ]),
             ),
             ("required", `List([`String("variable")])),
           ]),
         ),
       ]),
     ),
   ]);
   */

// todo: remove select_all by properly implementing goto navigators
let select_all: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("select_all")),
        (
          "description",
          `String(
            "Selects the entire sketch. Used in rare case where goto tools are not working.",
          ),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            ("properties", `Assoc([])),
            ("required", `List([])),
          ]),
        ),
      ]),
    ),
  ]);

let paste: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("paste")),
        (
          "description",
          `String(
            "Pastes the given code over whatever you currently have selected/highlighted.",
          ),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            (
              "properties",
              `Assoc([
                (
                  "code",
                  `Assoc([
                    ("type", `String("string")),
                    (
                      "description",
                      `String(
                        "The code to paste over whatever you currently have selected/highlighted.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("code")])),
          ]),
        ),
      ]),
    ),
  ]);

let delete: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("delete")),
        (
          "description",
          `String("Deletes all of the currently selected text."),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            ("properties", `Assoc([])),
            ("required", `List([])),
          ]),
        ),
      ]),
    ),
  ]);

let submit: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("submit")),
        (
          "description",
          `String(
            "Submits the task once you believe it to be complete, ending the iterative tool call and task completion process.",
          ),
        ),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            ("properties", `Assoc([])),
            ("required", `List([])),
          ]),
        ),
      ]),
    ),
  ]);

let get_few_shot_comp_examples = () => {
  "<fewShotExamples>The following are GOLDEN EXAMPLES from agents who successfully implemented user-requested features."
  ++ "Oh how you ASPIRE to be as elegant and efficient as they are! "
  ++ "In fact, YOU CAN BE! As long as you study what they've done oh-so-well!\n"
  ++ Ex_Simple_1.self
  ++ Ex_Simple_2.self
  ++ Ex_Tally.self
  ++ Ex_Comparator.self
  ++ Ex_Comparator_2.self
  ++ "</fewShotExamples>";
};

let self = instructions @ hazel_syntax_notes @ summarized_hazel_docs;
// @ [get_few_shot_comp_examples()];
