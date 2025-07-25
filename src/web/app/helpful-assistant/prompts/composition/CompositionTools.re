open Util;

// idea: allow for multiple variables to be selected at once
//       or rather, allow for beginning and end of selection
//       to be specified (based on variable definitons)

let go_to_parent: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("go_to_parent")),
        (
          "description",
          `String(
            "Moves the cursor to the parent node of the current node in the AST.",
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

let go_to_child: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("go_to_child")),
        (
          "description",
          `String(
            "Moves the cursor to the specified child node of the current node in the AST.",
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
                  "index",
                  `Assoc([
                    ("type", `String("integer")),
                    (
                      "description",
                      `String(
                        "The index of the child node to move the cursor to. Index is derived from the list of displayed children nodes of the current node.",
                      ),
                    ),
                  ]),
                ),
              ]),
            ),
            ("required", `List([`String("index")])),
          ]),
        ),
      ]),
    ),
  ]);

let view_definition: API.Json.t =
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String("view_definition")),
        (
          "description",
          `String("Displays the definition of the current node in the AST."),
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

/* OLD TOOLS as of 07/24/2025
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
                         `String(
                           "The name of the variable to be updated. Make sure to exclude the '^' and all characters after it.",
                         ),
                       ),
                     ]),
                   ),
                   (
                     "variable_id",
                     `Assoc([
                       ("type", `String("string")),
                       (
                         "description",
                         `String(
                           "The unique id of the variable to be updated. These are the characters after the '^'. ",
                         ),
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
                 `List([
                   `String("variable_name"),
                   `String("variable_id"),
                   `String("new_pattern"),
                 ]),
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
                           "The name of the variable to have its definition updated. Make sure to exclude the '^' and all characters after it.",
                         ),
                       ),
                     ]),
                   ),
                   (
                     "variable_id",
                     `Assoc([
                       ("type", `String("string")),
                       (
                         "description",
                         `String(
                           "The unique id of the variable to be updated. These are the characters after the '^'.",
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
                 `List([
                   `String("variable_name"),
                   `String("variable_id"),
                   `String("new_definition"),
                 ]),
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
                           "The name of the variable to have its body updated. Make sure to exclude the '^' and all characters after it.",
                         ),
                       ),
                     ]),
                   ),
                   (
                     "variable_id",
                     `Assoc([
                       ("type", `String("string")),
                       (
                         "description",
                         `String(
                           "The unique id of the variable to be updated. These are the characters after the '^'.",
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
                 `List([
                   `String("variable_name"),
                   `String("variable_id"),
                   `String("new_body"),
                 ]),
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
                           "The name of the variable to have its binding updated. Make sure to exclude the '^' and all characters after it.",
                         ),
                       ),
                     ]),
                   ),
                   (
                     "variable_id",
                     `Assoc([
                       ("type", `String("string")),
                       (
                         "description",
                         `String(
                           "The unique id of the variable to be updated. These are the characters after the '^'.",
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
                 `List([
                   `String("variable_name"),
                   `String("variable_id"),
                   `String("new_binding"),
                 ]),
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
                           "The name of the variable to have its body deleted. Make sure to exclude the '^' and all characters after it.",
                         ),
                       ),
                     ]),
                   ),
                   (
                     "variable_id",
                     `Assoc([
                       ("type", `String("string")),
                       (
                         "description",
                         `String(
                           "The unique id of the variable to be updated. These are the characters after the '^'.",
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
                   (
                     "variable_id",
                     `Assoc([
                       ("type", `String("string")),
                       (
                         "description",
                         `String(
                           "The unique id of the variable to be updated. These are the characters after the '^'.",
                         ),
                       ),
                     ]),
                   ),
                 ]),
               ),
               (
                 "required",
                 `List([`String("variable_name"), `String("variable_id")]),
               ),
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
                           "The name of the variable to have code added before its binding. Make sure to exclude the '^' and all characters after it.",
                         ),
                       ),
                     ]),
                   ),
                   (
                     "variable_id",
                     `Assoc([
                       ("type", `String("string")),
                       (
                         "description",
                         `String(
                           "The unique id of the variable to be updated. These are the characters after the '^'.",
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
                           "The name of the variable to have code added after its binding. Make sure to exclude the '^' and all characters after it.",
                         ),
                       ),
                     ]),
                   ),
                   (
                     "variable_id",
                     `Assoc([
                       ("type", `String("string")),
                       (
                         "description",
                         `String(
                           "The unique id of the variable to be updated. These are the characters after the '^'.",
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
                   (
                     "variable_id",
                     `Assoc([
                       ("type", `String("string")),
                       (
                         "description",
                         `String(
                           "The unique id of the variable to be updated. These are the characters after the '^'.",
                         ),
                       ),
                     ]),
                   ),
                 ]),
               ),
               (
                 "required",
                 `List([`String("variable_name"), `String("variable_id")]),
               ),
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
   */
