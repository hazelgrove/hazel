open Util;
open Haz3lcore;
open Language;

let get_sketch_and_error_ctx =
    (editor: CodeWithStatics.Model.t): list(string) => {
  let sketch_seg =
    Zipper.smart_seg(
      ~dump_backpack=true,
      ~erase_buffer=true,
      editor.editor.state.zipper,
    );
  let errors = ErrorPrint.all(editor.statics.info_map);
  let static_error_arr =
    switch (errors) {
    | [] => ["No static errors found"]
    | _ => errors
    };
  let ctx =
    [
      "PROGRAM SKETCH: ```"
      ++ ErrorPrint.Print.seg(~holes=Some("?"), sketch_seg)
      ++ "```",
    ]
    @ ["STATIC ERRORS: "]
    @ static_error_arr;
  ctx;
};

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
  let prelude = ["You are a helpful coding assistant in Hazel. \n"];

  let normal_suggestion_prompt = (completion_token: string) =>
    SuggestionPrompt_normal.self(completion_token);

  let cot_suggestion_prompt = (completion_token: string) =>
    SuggestionPrompt_cot.self(completion_token);

  let hazel_syntax_notes = HazelSyntaxNotes.self;

  let composition_prompt = CompositionPrompt.self;

  let summarized_hazel_docs = SummarizedHazelDocs.self;

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
                ? cot_suggestion_prompt(completion_token)
                : normal_suggestion_prompt(completion_token)
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
      : list(OpenRouter.message) =>
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
          [
            "sketch: ```"
            ++ ErrorPrint.Print.seg(~holes=Some("?"), sketch)
            ++ "```",
          ]
          @ get_static_context(
              options.expected_type,
              options.relevant_ctx,
              ci,
              hole_label,
            ),
        ),
      ),
    ];

  let add_suggestion =
      (
        ~response: string,
        ~tile: Id.t,
        ~schedule_action: Editors.Update.t => unit,
      ) => {
    let actions = [
      Action.Select(Tile(Id(tile, Direction.Left))),
      Action.Destruct(Direction.Left),
      Action.Insert(" "),
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
};

module Composition = {
  let max_tool_calls = 10;

  let statics_of_exp_seg =
      (init_ctx: Ctx.t, sketch: Segment.t): (Info.exp, Statics.Map.t) =>
    Statics.uexp_to_info_map(
      ~ctx=init_ctx,
      ~ancestors=[],
      MakeTerm.go(sketch).term,
      Id.Map.empty,
      ~duplicates=[],
      ~expected_labels=None,
      ~label_sort=false,
    );

  // Prompt with appropriate context for each message
  let mk_ctx_prompt =
      (options: Options.t, editor: CodeWithStatics.Model.t)
      : OpenRouter.message => {
    let _ = options; // TODO: Either remove params or update function to use params AnCRask
    OpenRouter.mk_user_msg(
      String.concat(
        "\n",
        get_sketch_and_error_ctx(editor)
        @ [
          "SELECTED CODE: "
          ++ (
            String.length(
              ErrorPrint.Print.seg(
                ~holes=Some("?"),
                editor.editor.state.zipper.selection.content,
              ),
            )
            == 0
              ? "None. Use a goto_* command to select a code segment."
              : "```"
                ++ ErrorPrint.Print.seg(
                     ~holes=Some("?"),
                     editor.editor.state.zipper.selection.content,
                   )
                ++ "```"
          ),
        ],
      ),
    );
  };

  type loc_of_edit =
    | Before
    | After
    | Current;

  type loc_of_goto =
    | Body
    | Definition
    | All;

  type code = string;

  type loc_of_add =
    | Before
    | After;

  type edit_action =
    | RenameVariable(code)
    | UpdateDefinition(code)
    | UpdateBody(code)
    | DeleteVariable
    | DeleteBody
    | Add(loc_of_add, code);

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
  let apply_edit_action =
      (
        ~ed: CodeWithStatics.Model.t,
        ~edit_action: edit_action,
        ~variable_name: option(string),
        ~schedule_action: Editors.Update.t => unit,
      )
      : unit => {
    let actions =
      switch (variable_name) {
      | Some(variable_name) =>
        let statics = CodeWithStatics.Model.get_statics(ed);
        // Find the first matching variable in the context using fold
        // TODO: Handle shadowed variables
        let matching_id =
          Id.Map.fold(
            (_, info, acc) => {
              switch (acc) {
              | Some(_) => acc // Already found a match
              | None =>
                let ctx = Info.ctx_of(info);
                switch (Ctx.lookup_var(ctx, variable_name)) {
                | Some(entry) => Some(entry.id)
                | None =>
                  switch (Ctx.lookup_tvar_id(ctx, variable_name)) {
                  | Some(id) => Some(id)
                  | None => None
                  }
                };
              }
            },
            statics.info_map,
            None,
          );

        print_endline("here #1");
        let var_info =
          switch (matching_id) {
          | Some(id) => Id.Map.find_opt(id, statics.info_map)
          | None => raise(Failure("Variable not found in context"))
          };
        print_endline("here #2");

        let rec lowest_enclosing_id = (ancestors: list(Id.t)) => {
          switch (ancestors) {
          | [] => (Id.invalid, Id.invalid, Id.invalid)
          | [hd_anc, ...rem_ancs] =>
            switch (Id.Map.find_opt(hd_anc, statics.info_map)) {
            | Some(hd_anc_term) =>
              print_endline("hd_anc: " ++ Id.show(hd_anc));
              switch (Info.any_of(hd_anc_term)) {
              | Some(Exp(exp)) =>
                switch (Exp.term_of(exp)) {
                | TyAlias(var, def, body) => (
                    TPat.rep_id(var),
                    Typ.rep_id(def),
                    Exp.rep_id(body),
                  )
                | Let(var, def, body) => (
                    Pat.rep_id(var),
                    Exp.rep_id(def),
                    Exp.rep_id(body),
                  )
                /* todo: figure out how to find hinted test from up above matching_id
                   | HintedTest(_, _) => lowest_enclosing_id(rem_ancs)
                   | Seq(e1, e2) => (
                       Exp.rep_id(e1),
                       Id.invalid,
                       Exp.rep_id(e2),
                     )
                   */
                // Not a definition binding, recurse
                | _ => lowest_enclosing_id(rem_ancs)
                }
              | _ => lowest_enclosing_id(rem_ancs)
              };
            | _ => lowest_enclosing_id(rem_ancs)
            }
          };
        };

        let (var, def, body) =
          switch (var_info) {
          | Some(info) =>
            let ancestors = Info.ancestors_of(info);
            print_endline("Here #4");
            lowest_enclosing_id(ancestors);
          | None =>
            print_endline("No var info found");
            (Id.invalid, Id.invalid, Id.invalid);
          };

        switch (edit_action) {
        | RenameVariable(new_variable_name) => [
            Action.Select(Assistant(Var(var))),
            Action.Paste(String(new_variable_name)),
          ]
        | UpdateDefinition(new_definition) => [
            Action.Select(Assistant(Def(def))),
            Action.Paste(String(new_definition)),
          ]
        | UpdateBody(new_body) => [
            Action.Select(Assistant(Body(body))),
            Action.Paste(String(new_body)),
          ]
        | DeleteVariable => [
            Action.Select(Assistant(VarDef(var))),
            Action.Paste(String("")),
          ]
        | DeleteBody => [
            Action.Select(Assistant(Body(body))),
            Action.Paste(String("")),
          ]
        | Add(loc, code) =>
          switch (loc) {
          | Before => [
              Action.Select(Assistant(VarDef(var))),
              Action.Move(Local(Left(ByToken))),
              Action.Paste(String(code)),
            ]
          | After => [
              Action.Select(Assistant(VarDef(var))),
              Action.Move(Local(Right(ByToken))),
              Action.Paste(String(code)),
            ]
          }
        };
      | None =>
        switch (edit_action) {
        | Add(loc, code) =>
          switch (loc) {
          | Before => [
              Action.Move(Extreme(Up)),
              Action.Paste(String(code)),
            ]
          | After => [
              Action.Move(Extreme(Down)),
              Action.Paste(String(code)),
            ]
          }
        | _ =>
          print_endline(
            "Error applying assistant edit action: No variable name provided",
          );
          [];
        }
      };

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
