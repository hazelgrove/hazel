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
  let prelude = ["You are a helpful coding assistant in Hazel. \n"];

  let normal_suggestion_prompt = (completion_token: string) =>
    SuggestionPrompt_normal.self(completion_token);

  let cot_suggestion_prompt = (completion_token: string) =>
    SuggestionPrompt_cot.self(completion_token);

  let hazel_syntax_notes = HazelSyntaxNotes.self;

  let task_completion_toolkit = TaskCompletionToolKit.self;

  let summarized_hazel_docs = SummarizedHazelDocs.self;

  let few_shot_composition_examples = FewShotCompositionExamples.self;

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
};

module Composition = {
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
      (options: Options.t, sketch: Segment.t, editor: CodeWithStatics.Model.t)
      : string => {
    let _ = options; // TODO: Either remove params or update function to use params

    //let (_, info_map) = statics_of_exp_seg(Info.ctx_of(ci), sketch);
    let errors = ErrorPrint.all(editor.statics.info_map);

    let static_error_arr =
      switch (errors) {
      | [] => ["No static errors found"]
      | _ => errors
      };
    String.concat(
      "\n",
      [
        "PROGRAM SKETCH: ```"
        ++ ErrorPrint.Print.seg(~holes=Some("?"), sketch)
        ++ "```",
      ]
      @ ["STATIC ERRORS: "]
      @ static_error_arr
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
            : ErrorPrint.Print.seg(
                ~holes=Some("?"),
                editor.editor.state.zipper.selection.content,
              )
        ),
      ],
    );
  };

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
        // Unselect current definition
        Action.Unselect(Some(Left)),
        // Paste new code
        Action.Paste(String(code ++ "\n")),
      ]
    | After => [
        // Unselect current definition
        Action.Unselect(Some(Direction.Right)),
        // Paste new code
        Action.Paste(String("\n" ++ code)),
      ]
    | Current =>
      String.length(code) == 0
        ? [
          // This implies the calling of the ```delete``` tool
          // Replace current definition
          Action.Paste(String(code)),
          // Destruct left
          Action.Destruct(Left),
        ]
        : [
          // Replace current definition
          Action.Paste(String(code)),
        ]
    // We paste the code edit, then reselect the definition, and copy
    // to clipboard shim to give context to assistant.
    };
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
