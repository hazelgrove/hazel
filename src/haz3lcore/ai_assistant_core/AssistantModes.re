open Util;
open Language;
open Language.Statics;

/*
    Modules organizing out functionalities for each mode the AI assistant can operate in.
 */

module Completion = {
  /*
   --- Completion Mode ---
     LLM-suggested hole completions, based strongly on https://hazel.org/papers/chatlsp-oopsla2024.pdf
   */
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
        ~schedule_action: Editor.Update.t => unit,
      ) => {
    let actions = [
      Action.Select(Tile(Id(tile, Direction.Left))),
      Action.Buffer(Set(LLM(response))),
    ];
    // Apply each action in sequence
    List.iter(action => {schedule_action(action)}, actions);
  };

  module ErrorRound = {
    open OptUtil.Syntax;
    module StringSet = Set.Make(String);

    let get_parse_errs =
        (sketch_z: Zipper.t, completion: string): Result.t(Zipper.t, string) =>
      //NOTE: This function is pretty basic; reporting approach could be improved
      /* For now we required that the completion be complete in-itself: */
      switch (Parser.to_zipper(~zipper_init=Zipper.init(), completion)) {
      | None => Error("Undocumented parse error, no feedback available")
      | Some(completion_z) =>
        switch (Zipper.local_backpack(completion_z)) {
        | [_, ..._] as orphans =>
          let orphans =
            List.map(
              (tile: Tile.t) =>
                String.concat("", Tile.effective_label(tile)),
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
              Zipper.insert_segment(sketch_z, segment);
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

  let check_req =
      (
        ~schedule_action: AssistantUpdateAction.t => unit,
        ~schedule_setting: AssistantSettings.action => unit,
        ~z: Zipper.t,
        ~chat_id: Id.t,
      )
      : unit => {
    let caret = z.caret;
    let send_message = (tile_id, advanced_reasoning) => {
      schedule_setting(AssistantSettings.SwitchMode(CodeSuggestion));
      schedule_action(
        AssistantUpdateAction.SendMessage(
          Completion(Request(tile_id, advanced_reasoning)),
          None,
          chat_id,
        ),
      );
    };

    // Check if user just typed ??
    switch (caret, Zipper.neighbor_tokens(z)) {
    | (Outer, (_, Some("??")))
    | (Outer, (Some("??"), _)) =>
      let tileId = Option.get(Indicated.index(z));
      let advanced_reasoning = false;
      send_message(tileId, advanced_reasoning);
    | (Outer, (_, Some("?a")))
    | (Outer, (Some("?a"), _)) =>
      let tileId = Option.get(Indicated.index(z));
      let advanced_reasoning = true;
      send_message(tileId, advanced_reasoning);
    | _ => ()
    };
  };
};

module Composition = {
  /*
   --- Composition Mode ---
     LLM-based agentic code sysnthesis. Differs from code completion in that it can
     navigate the program structure, and perform more complex, multi-step edits.
   */
  open AssistantTreeHelper.HighLevelNode;
  let max_tool_calls = 40;

  // Prompt with appropriate AST context for each message.
  // The default information as of now is as follows:
  //
  // Current node: <name>
  // Parent node: <name>
  // Children nodes: [<name>, <name>, ...]
  // Static errors: <errors>
  let mk_structured_code_map_prompt =
      (_: ChatLSP.Options.t, _z: Zipper.t, _info_map: Statics.Map.t)
      : (OpenRouter.message, AssistantModel.display) => {
    (
      OpenRouter.mk_user_msg("TODO - UNIMPLEMENTED"),
      {
        displayable_content: [Text("TODO - UNIMPLEMENTED")],
        raw_content: "TODO - UNIMPLEMENTED",
        collapsed: true,
      },
    );
  };

  let mk_structure_edit_msg = (~tool_call: OpenRouter.tool_call): string =>
    // AddToolLabel_3.0: what should the text content of this tool call to the user be?
    //                   (not to the llm, that is the string returned in AssistantModes.Composition.apply_action)
    try({
      let tool_name = tool_call.tool_name;
      let args = tool_call.args;
      let action =
        CompositionTools.action_of(
          ~tool_name,
          ~args=API.Json.get_string_kvs(args),
        );
      let _enclose_in_backticks = (str: string) => "```" ++ str ++ "```";
      "Agent called tool: " ++ CompositionTools.string_of(action);
    }) {
    | Failure(err) =>
      "The agent may have called tools with invalid arguments: " ++ err
    | Invalid_argument(e) =>
      "The argument map creation may have failed, or some other fatal issue occurred: "
      ++ e
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

  // AddToolLabel_2.0: handle the effects of the action on the editor itself
  let apply_editor_action =
      (
        ~z: Zipper.t,
        ~info_map: Id.Map.t(Info.t),
        ~action: CompositionTools.action,
        ~schedule_editor_action: Editor.Update.t => unit,
        ~schedule_tool_response: AssistantUpdateAction.status => unit,
      )
      : unit => {
    switch (action) {
    | Read(r) =>
      let res =
        switch (r) {
        | ViewEntireDefintion =>
          switch (build(z, info_map)) {
          | Some(node_info) => CompositionView.full_definition(z, node_info)
          | None => "Failed to derive full definition"
          }
        | ShowUseSites => "todo"
        };
      schedule_tool_response(Success(res));
    | _ =>
      schedule_editor_action(
        Action.Composition((action, Some(schedule_tool_response))),
      )
    };
  };
};

module Tutor = {
  /*
   --- Tutor Mode ---
     Empty.
     Tutor mode is just an LLM chat prompted with hazel-specific information.
   */
};
