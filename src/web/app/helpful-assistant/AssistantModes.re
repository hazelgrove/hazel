open Util;
open Haz3lcore;
open Language;
open Language.Statics;

// --- Completion Mode ---
// LLM-suggested hole completions, based strongly on https://hazel.org/papers/chatlsp-oopsla2024.pdf
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

  let mk_const_prompt =
      (
        options: ChatLSP.Options.t,
        hole_label: string,
        advanced_reasoning: bool,
      )
      : OpenRouter.message => {
    let prompt =
      String.concat(
        "\n",
        [
          ChatLSP.SystemPrompt.mk_suggestion_prompt(
            options,
            hole_label,
            advanced_reasoning,
          ),
        ]
        @ CompletionExamples.get(
            options.num_examples,
            hole_label,
            advanced_reasoning,
          ),
      );
    OpenRouter.mk_system_msg(prompt);
  };

  let mk_ctx_prompt =
      (
        options: ChatLSP.Options.t,
        ci: Info.t,
        sketch: Segment.t,
        hole_label: string,
      )
      : OpenRouter.message =>
    OpenRouter.mk_user_msg(
      String.concat(
        "\n",
        ["sketch: ```" ++ ErrorPrint.Print.seg(~holes="?", sketch) ++ "```"]
        @ get_static_context(
            options.expected_type,
            options.relevant_ctx,
            ci,
            hole_label,
          ),
      ),
    );

  let add_suggestion =
      (
        ~response: string,
        ~tile: Id.t,
        ~schedule_action: Editors.Update.t => unit,
      ) => {
    let actions = [
      Action.Select(Tile(Id(tile, Direction.Left))),
      Action.Buffer(Set(LLM(response))),
    ];
    // Apply each action in sequence
    List.iter(
      action => {
        let perform_action = CodeEditable.Update.Perform(action);
        let cell_action = CellEditor.Update.MainEditor(perform_action);
        let scratch_action = Editors.Update.Scratch(CellAction(cell_action));
        schedule_action(scratch_action);
      },
      actions,
    );
  };

  module ErrorRound = {
    open OptUtil.Syntax;
    module StringSet = Set.Make(String);

    let get_parse_errs =
        (sketch_z: Zipper.t, completion: string): Result.t(Zipper.t, string) =>
      //NOTE: This function is pretty basic; reporting approach could be improved
      /* For now we required that the completion be complete in-itself: */
      switch (Perform.paste(Zipper.init(), completion)) {
      | None => Error("Undocumented parse error, no feedback available")
      | Some(completion_z) =>
        switch (completion_z.backpack) {
        | [_, ..._] as orphans =>
          let orphans =
            List.map(
              (s: Selection.t) => Printer.of_segment(~holes="", s.content),
              orphans,
            );
          Error(
            "The parser has detected unmatched delimiters. (The presence of a '=>' in the list likely indicates that a '->' was mistakingly used in a case expression). Unmatched delimiters: "
            ++ String.concat(", ", orphans),
          );
        | [] =>
          let segment = Zipper.zip(completion_z);
          switch (
            {
              let* sketch_z = Destruct.go(Left, sketch_z);
              let+ sketch_z = Destruct.go(Left, sketch_z);
              Perform.paste_segment(sketch_z, segment);
            }
          ) {
          | None => Error("Undocumented parse error, no feedback available")
          | Some(completion_z) => Ok(completion_z)
          };
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
};

// --- Composition Mode ---
// LLM-based agentic code sysnthesis. Differs from code completion in that it can
// navigate the program structure, and perform more complex, multi-step edits.
module Composition = {
  let max_tool_calls = 10;

  // Prompt with appropriate AST context for each message.
  // The default information as of now is as follows:
  //
  // Current node: <name>
  // Parent node: <name>
  // Children nodes: [<name>, <name>, ...]
  // Static errors: <errors>
  let mk_ctx_prompt =
      (_: ChatLSP.Options.t, editor: CodeWithStatics.Model.t)
      : OpenRouter.message => {
    let curr_node_info = TreeHelper.build_sub_AST(editor);

    let curr_node_str = "Current node: " ++ curr_node_info.name;
    let parent_node_str =
      switch (curr_node_info.parent) {
      | Some(parent) => "Parent node: " ++ parent.name
      | None => "No parent node, you are at the root of the program's AST."
      };
    let siblings_nodes_str =
      "Sibling nodes: ["
      ++ String.concat(
           ", ",
           List.mapi(
             (index, node: TreeHelper.node) =>
               node.name ++ " (index: " ++ string_of_int(index) ++ ")",
             curr_node_info.siblings,
           ),
         )
      ++ "]";
    // let children_nodes_str =
    //   "Child nodes: ["
    //   ++ String.concat(
    //        ", ",
    //        List.mapi(
    //          (index, node: option(TreeHelper.node)) =>
    //            Option.get(node).name
    //            ++ " (index: "
    //            ++ string_of_int(index)
    //            ++ ")",
    //          children_nodes,
    //        ),
    //      )
    //   ++ "]";

    let definition_str =
      "Definition of \""
      ++ curr_node_info.name
      ++ "\":\n```"
      ++ TreeHelper.View.definition(
           editor.editor.state.zipper,
           curr_node_info,
         )
      ++ "```";

    let static_errors = ErrorPrint.all(editor.statics.info_map);
    let static_errors_str =
      switch (static_errors) {
      | [] => "No static errors found in the program."
      | _ => "Static errors: " ++ String.concat(", ", static_errors)
      };

    let ast_info_str =
      String.concat(
        "\n",
        [
          "<AST information>",
          curr_node_str,
          parent_node_str,
          siblings_nodes_str,
          "</AST information>",
        ],
      );

    let sketch_info_str =
      String.concat(
        "\n",
        [
          "<Sketch information>",
          definition_str,
          static_errors_str,
          "</Sketch information>",
        ],
      );

    OpenRouter.mk_user_msg(
      String.concat("\n", [ast_info_str, sketch_info_str]),
    );
  };

  /*
   * ------------------------------
   *  Structure-Based Action Language
   * ------------------------------
   */

  type code = string;
  type variable = string;

  // --- Navigation Actions ---
  // These actions are used to navigate the AST, and do not modify the program
  // or provide additional information to the LLM. They strictly move the cursor
  // through the AST.

  type nav_action =
    // Goes to the parent node of the current node in the AST
    | GoToParent
    // Goes to the child node of the current node in the AST
    | GoToChild(string, option(int))
    // Jumps to the root node of the AST
    | GoToSibling(string, option(int));
  // todo (above): using indices for children/siblings navigation
  // seems a little arbitrary, and should probably default to using
  // the names themselces
  // idea: still display indices to LLM, ask for string variable names,
  // but allow for optional integer index argument to handle cases of shadowing

  // --- File-Read Actions ---
  // These actions are used purely to read information from the program,
  // and do not modify the program or the cursor location in the AST.

  type read_action =
    // Displays the definition of the current node in the AST
    | ViewDefinition
    // Peeks at the definition of the specified variable
    | PeekDefinition(variable)
    // Shows the path from the root node to the current node in the AST
    | ShowPath
    // Shows the siblings of the current node in the AST
    | ShowSiblings;

  // --- Edit Actions ---
  // These actions are used to modify the program. They do provide additional
  // information to the LLM (via reading), but may move the cursor (eg. removing
  // a node will require the cursor to be moved elsewhere).

  type edit_action =
    // Updates the definition of the current node in the AST
    | Update(code)
    // Removes the current node from the AST
    | Remove
    // Inserts a new node in the AST
    | Insert(code);

  type action =
    | Nav(nav_action)
    | Read(read_action)
    | Edit(edit_action);

  type result = string;

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

  // Helper function for applying a list of editor-perform actions to the editor
  let schedule_actions =
      (~actions: list(Action.t), ~schedule_action: Editors.Update.t => unit) => {
    List.iter(
      action => {
        let perform_action = CodeEditable.Update.Perform(action);
        let cell_action = CellEditor.Update.MainEditor(perform_action);
        let scratch_action = Editors.Update.Scratch(CellAction(cell_action));
        schedule_action(scratch_action);
      },
      actions,
    );
  };

  // AddToolLabel_2
  let apply_action =
      (
        ~editor: CodeWithStatics.Model.t,
        ~action: action,
        ~schedule_action: Editors.Update.t => unit,
        ~curr_node_info: TreeHelper.node,
      )
      : result => {
    let schedule_actions = (actions: list(Action.t)) =>
      schedule_actions(~actions, ~schedule_action);

    switch (action) {
    // Navigate to the parent node of the current node
    | Nav(nav_action) =>
      switch (nav_action) {
      | GoToParent =>
        switch (curr_node_info.parent) {
        | None => raise(Failure("This node does not have a parent"))
        | Some(parent) =>
          let actions = [
            Action.Select(
              Tile(Id(Info.id_of(parent.info), Direction.Right)),
            ),
          ];
          schedule_actions(actions);
          "Cursor moved from \""
          ++ curr_node_info.name
          ++ "\" to its parent \""
          ++ parent.name
          ++ "\"";
        }
      | GoToChild(who, where) =>
        // todo/idea: move candidates out here, maybe change indexing method?
        // to assert referencing by both name and index...
        let child =
          switch (where) {
          | None =>
            let candidates =
              List.filter(
                (child: TreeHelper.node) => child.name == who,
                curr_node_info.children,
              );
            if (List.length(candidates) > 1) {
              raise(
                Failure(
                  "Multiple children found, not sure how to resolve ambiguity. Please specify which child to reference via using the index associated with that child.",
                ),
              );
            };
            switch (ListUtil.hd_opt(candidates)) {
            | None =>
              raise(
                Failure(
                  "Child not found. Make sure the current node has children, and that the child you're referencing exists.",
                ),
              )
            | Some(child) => child
            };
          | Some(here) =>
            switch (List.nth_opt(curr_node_info.children, here)) {
            | None =>
              raise(
                Failure(
                  "Child index out of bounds. Make sure the current node has children, and that your given index is within bounds.",
                ),
              )
            | Some(child) => child
            }
          };
        schedule_actions([
          Action.Select(Tile(Id(Info.id_of(child.info), Direction.Right))),
        ]);
        "Cursor moved from \""
        ++ curr_node_info.name
        ++ "\" to its child \""
        ++ child.name
        ++ "\"";
      | GoToSibling(who, where) =>
        let sibling =
          switch (where) {
          | None =>
            let candidates =
              List.filter(
                (sibling: TreeHelper.node) => sibling.name == who,
                curr_node_info.siblings,
              );
            if (List.length(candidates) > 1) {
              raise(
                Failure(
                  "Multiple siblings found, not sure how to resolve ambiguity. Please specify which sibling to reference via using the index associated with that sibling.",
                ),
              );
            };
            switch (ListUtil.hd_opt(candidates)) {
            | None =>
              raise(
                Failure(
                  "Sibling not found. Make sure the current node has siblings, and that the sibling you're referencing exists.",
                ),
              )
            | Some(sibling) => sibling
            };
          | Some(here) =>
            switch (List.nth_opt(curr_node_info.siblings, here)) {
            | None =>
              raise(
                Failure(
                  "Sibling index out of bounds. Make sure the current node has siblings, and that your given index is within bounds.",
                ),
              )
            | Some(sibling) => sibling
            }
          };
        schedule_actions([
          Action.Select(
            Tile(Id(Info.id_of(sibling.info), Direction.Right)),
          ),
        ]);
        "Cursor moved from \""
        ++ curr_node_info.name
        ++ "\" to its sibling \""
        ++ sibling.name
        ++ "\"";
      }
    | Read(read_action) =>
      switch (read_action) {
      | ViewDefinition =>
        "Definition of \""
        ++ curr_node_info.name
        ++ "\":\n```"
        ++ TreeHelper.View.definition(
             editor.editor.state.zipper,
             curr_node_info,
           )
        ++ "```"
      | _ => raise(Failure("Unhandled read action"))
      }
    | Edit(_) => raise(Failure("Unhandled edit action"))
    };
  };
};

// --- Tutor Mode ---
module Tutor = {
  // Empty module for now
  // Tutor mode is pretty simple, and basically just an LLM chat
  // prompted with hazel-specific information.
};
