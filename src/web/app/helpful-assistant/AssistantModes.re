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
  let mk_local_code_map_prompt =
      (_: ChatLSP.Options.t, editor: CodeWithStatics.Model.t)
      : (OpenRouter.message, AssistantModel.display) => {
    print_endline(
      "here #a before building sub AST in mk_local_code_map_prompt",
    );
    let curr_node_info =
      AssistantTreeHelper.build_curr_node_info(
        editor.editor.state.zipper,
        editor.statics.info_map,
      );
    print_endline(
      "here #b after building sub AST in mk_local_code_map_prompt",
    );

    switch (curr_node_info) {
    | None =>
      // Special case: No let or type alias expressions in the program.
      // Just dump selection. It is assumed that the entire sketch is selected in this case.
      let sketch_seg = editor.editor.state.zipper.selection.content;
      let sketch_seg_str = Printer.of_segment(~holes="?", sketch_seg);
      let sketch_seg_hd_str = "No let or type alias expressions found in the program, unable to derive any meaningful AST information. Selecting the entire program:\n```";
      let sketch_seg_tl_str = "```";
      let sketch_str =
        String.concat(
          "\n",
          [sketch_seg_hd_str, sketch_seg_str, sketch_seg_tl_str],
        );

      let static_errors = ErrorPrint.all(editor.statics.info_map);
      let static_errors_str =
        switch (static_errors) {
        | [] => "\nNo static errors found in the program."
        | _ => "\nStatic errors: " ++ String.concat(", ", static_errors)
        };

      let sketch_info_str =
        String.concat(
          "\n",
          [
            "<Sketch information>",
            sketch_str,
            static_errors_str,
            "</Sketch information>",
          ],
        );
      let local_code_map_str = sketch_info_str;

      (
        OpenRouter.mk_user_msg(local_code_map_str),
        {
          displayable_content: [
            Text(sketch_seg_hd_str),
            //Code(sketch_seg), // todo: there's a skel failure happening here
            Text(sketch_seg_tl_str ++ static_errors_str),
          ],
          raw_content: local_code_map_str,
          collapsed: true,
        },
      );
    | Some(curr_node_info) =>
      let curr_node_str = "Current node: " ++ curr_node_info.name;
      let parent_node_str =
        switch (curr_node_info.parent) {
        | Some(parent) =>
          "Path to node: "
          ++ AssistantTreeHelper.get_path_to_node(
               parent.info,
               editor.statics.info_map,
             )
        | None => "No parent node, you are at the top level of the program's AST."
        };
      let siblings_nodes_str =
        "Sibling nodes: ["
        ++ String.concat(
             ", ",
             List.mapi(
               (index, node: AssistantTreeHelper.node) =>
                 node.name ++ " (index: " ++ string_of_int(index) ++ ")",
               curr_node_info.siblings,
             ),
           )
        ++ "]";
      let children_nodes_str =
        "Child nodes: ["
        ++ String.concat(
             ", ",
             List.mapi(
               (index, node: AssistantTreeHelper.node) =>
                 node.name ++ " (index: " ++ string_of_int(index) ++ ")",
               curr_node_info.children,
             ),
           )
        ++ "]";

      let sketch_seg =
        CompositionUtil.View.definition(
          editor.editor.state.zipper,
          curr_node_info,
        );

      let sketch_seg_hd_str =
        "Definition of \""
        ++ curr_node_info.name
        ++ "\"'s parent "
        ++ (
          switch (curr_node_info.parent) {
          | Some(parent) => "\"" ++ parent.name ++ "\""
          | None => "(no parent, displaying entire top level of the program)"
          }
        )
        ++ "\":\n```";
      let sketch_seg_str =
        Printer.of_segment(~holes="?", ~special_folds=true, sketch_seg);
      let sketch_seg_tl_str = "```";
      let def_str =
        String.concat(
          "\n",
          [sketch_seg_hd_str, sketch_seg_str, sketch_seg_tl_str],
        );

      let static_errors = ErrorPrint.all(editor.statics.info_map);
      let static_errors_str =
        switch (static_errors) {
        | [] => "\nNo static errors found in the program."
        | _ => "\nStatic errors: " ++ String.concat(", ", static_errors)
        };

      let ast_info_str =
        String.concat(
          "\n",
          [
            "<AST information>",
            curr_node_str,
            parent_node_str,
            siblings_nodes_str,
            children_nodes_str,
            "</AST information>",
          ],
        );

      let sketch_info_str =
        String.concat(
          "\n",
          [
            "<Sketch information>",
            def_str,
            static_errors_str,
            "</Sketch information>",
          ],
        );

      let local_code_map_str =
        String.concat("\n", [ast_info_str, sketch_info_str]);

      (
        OpenRouter.mk_user_msg(local_code_map_str),
        {
          displayable_content: [
            Text(ast_info_str ++ sketch_seg_hd_str),
            //Code(sketch_seg), //todo: avoid skel failures
            Text(sketch_seg_tl_str ++ static_errors_str),
          ],
          raw_content: local_code_map_str,
          collapsed: true,
        },
      );
    };
  };

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

  // AddToolLabel_2.0: handle the effects of the action on the editor itself
  let apply_action =
      (
        ~editor: CodeWithStatics.Model.t,
        ~action: CompositionTools.action,
        ~schedule_action: Editors.Update.t => unit,
      )
      : result => {
    let (result, actions) =
      CompositionTools.derive_actions(
        editor.editor.state.zipper,
        editor.statics.info_map,
        action,
      );
    // Apply actions to the editor
    schedule_actions(~actions, ~schedule_action);
    // Return the result (tool call response)
    result;
  };
};

// --- Tutor Mode ---
module Tutor = {
  // Empty module for now
  // Tutor mode is pretty simple, and basically just an LLM chat
  // prompted with hazel-specific information.
};
