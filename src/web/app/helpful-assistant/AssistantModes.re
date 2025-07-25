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
    let ast = ChatLSP.build_AST(editor);
    let curr_node = Option.get(ChatLSP.get_curr_node(editor, ast));

    let parent_node = Id.Map.find_opt(curr_node.parent, ast);
    let children_nodes =
      List.map(Id.Map.find_opt(_, ast), curr_node.children);

    let curr_node_str = "Current node: " ++ curr_node.name;
    let parent_node_str =
      switch (parent_node) {
      | Some(node) => "Parent node: " ++ node.name
      | None => "No parent node, you are at the root of the program's AST."
      };
    let children_nodes_str =
      "Children nodes: ["
      ++ String.concat(
           ", ",
           List.mapi(
             (index, node: option(ChatLSP.node)) =>
               Option.get(node).name
               ++ " (index: "
               ++ string_of_int(index)
               ++ ")",
             children_nodes,
           ),
         )
      ++ "]";
    let curr_depth_str =
      "Current depth in AST: " ++ string_of_int(curr_node.level);

    let static_errors = ErrorPrint.all(editor.statics.info_map);
    let static_errors_str =
      switch (static_errors) {
      | [] => "No static errors found in the program."
      | _ => "Static errors: " ++ String.concat(", ", static_errors)
      };

    OpenRouter.mk_user_msg(
      String.concat(
        "\n",
        [
          curr_node_str,
          parent_node_str,
          children_nodes_str,
          curr_depth_str,
          static_errors_str,
        ],
      ),
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
    | GoToChild(int)
    // Jumps to the root node of the AST
    | JumpToRoot;

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

  let get_definition =
      (
        editor: CodeWithStatics.Model.t,
        ast: ChatLSP.ast,
        curr_node: ChatLSP.node,
      ) => {
    let rec replace_term_with_ellipsis = (z: Zipper.t, ids: list(Id.t)) => {
      switch (ids) {
      | [] => z
      | [id, ...rest] =>
        let z' =
          ChatLSP.perform(Action.Select(Term(Id(id, Direction.Right))), z);
        switch (z') {
        | Ok(z') =>
          let z'' =
            ChatLSP.perform(
              Action.Project(SetIndicated(Specific(Fold))),
              z',
            );
          switch (z'') {
          | Ok(z'') => replace_term_with_ellipsis(z'', rest)
          | _ => replace_term_with_ellipsis(z', rest)
          };
        | _ => replace_term_with_ellipsis(z, rest)
        };
      };
    };
    let get_def_id_of_let = (term: Info.t): Id.t => {
      switch (term) {
      | InfoExp({term, _}) =>
        switch (Exp.term_of(term)) {
        | Let(_, def, _) => Exp.rep_id(def)
        | _ => Id.invalid
        }
      | _ => Id.invalid
      };
    };
    let z = editor.editor.state.zipper;

    let children_ids = curr_node.children;
    let children = List.map(Id.Map.find(_, ast), children_ids);
    let children_def_ids =
      List.map((c: ChatLSP.node) => get_def_id_of_let(c.self), children);
    let z = replace_term_with_ellipsis(z, children_def_ids);
    let z' =
      switch (
        ChatLSP.perform(
          Action.Select(
            Tile(Id(ChatLSP.id_of(curr_node), Direction.Right)),
          ),
          z,
        )
      ) {
      | Ok(z') => z'
      | _ => z
      };
    let seg = z'.selection.content;
    Printer.of_segment(~holes="?", ~special_folds=true, seg);
  };

  let apply_action =
      (
        ~editor: CodeWithStatics.Model.t,
        ~action: action,
        ~schedule_action: Editors.Update.t => unit,
        ~ast: ChatLSP.ast,
        ~curr_node: option(ChatLSP.node),
      )
      : result => {
    let schedule_actions = (actions: list(Action.t)) =>
      schedule_actions(~actions, ~schedule_action);

    switch (curr_node) {
    | None => raise(Failure("No current node found"))
    | Some(curr_node) =>
      switch (action) {
      // Navigate to the parent node of the current node
      | Nav(nav_action) =>
        switch (nav_action) {
        | GoToParent =>
          let actions = [
            Action.Select(Tile(Id(curr_node.parent, Direction.Right))),
          ];
          schedule_actions(actions);
          let parent_node = Id.Map.find(curr_node.parent, ast);
          "Cursor moved from \""
          ++ curr_node.name
          ++ "\" to \""
          ++ parent_node.name
          ++ "\"";
        | GoToChild(which) =>
          let child = List.nth(curr_node.children, which);
          schedule_actions([
            Action.Select(Tile(Id(child, Direction.Right))),
          ]);
          let child_node = Id.Map.find(child, ast);
          "Cursor moved from \""
          ++ curr_node.name
          ++ "\" to \""
          ++ child_node.name
          ++ "\"";
        | _ => raise(Failure("Unhandled nav action"))
        }
      | Read(read_action) =>
        switch (read_action) {
        | ViewDefinition =>
          "Definition of \""
          ++ curr_node.name
          ++ "\" (with child definitions collapsed with '...') is:\n```"
          ++ get_definition(editor, ast, curr_node)
          ++ "```"
        | _ => raise(Failure("Unhandled read action"))
        }
      | Edit(_) => raise(Failure("Unhandled edit action"))
      }
    };
  };
};

// --- Tutor Mode ---
module Tutor = {
  // Empty module for now
  // Tutor mode is pretty simple, and basically just an LLM chat
  // prompted with hazel-specific information.
};
