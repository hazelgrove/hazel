open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Util_web;

/* The exercises mode interface for a single exercise. Composed of multiple editors and results. */

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type editing_flags = {
    editing_title: bool,
    editing_prompt: bool,
    editing_test_val_rep: bool,
    editing_mut_test_rep: bool,
    editing_impl_grd_rep: bool,
    editing_module_name: bool,
    editing_syntax_rep: bool,
  };

  let editing_flags_false = {
    editing_title: false,
    editing_prompt: false,
    editing_test_val_rep: false,
    editing_mut_test_rep: false,
    editing_impl_grd_rep: false,
    editing_module_name: false,
    editing_syntax_rep: false,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    spec: CodeExercise.spec, // The spec that the model will be reset to on ResetExercise
    /* We keep a separate editors field below (even though each cell technically also has its own editor)
       for two reasons:
          1. There are two synced cells that have the same internal `editor` model
          2. The editors need to be `stitched` together before any cell calculations can be done */
    editors: CodeExercise.p(Editor.t),
    cells: CodeExercise.stitched(CellEditor.Model.t),
    editing_flags,
  };

  let of_spec = (~settings as _, ~instructor_mode as _: bool, spec) => {
    let editors =
      CodeExercise.map(
        spec,
        Editor.Model.mk(~root=Exp),
        Editor.Model.mk(~root=Exp),
      );
    let term_item_to_cell =
        (item: CodeExercise.TermItem.t): CellEditor.Model.t => {
      CellEditor.Model.mk(item.editor);
    };
    let cells =
      CodeExercise.stitch_term(editors)
      |> CodeExercise.map_stitched(_ => term_item_to_cell);
    {
      spec,
      editors,
      cells,
      editing_flags: editing_flags_false,
    };
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = CodeExercise.persistent_state;

  let persist = (exercise: t, ~instructor_mode: bool) =>
    CodeExercise.persist({eds: exercise.editors}, ~instructor_mode);

  let unpersist = (~instructor_mode, spec, persistent) => {
    let editors =
      CodeExercise.unpersist(~spec, ~instructor_mode, persistent).eds;
    let term_item_to_cell =
        (item: CodeExercise.TermItem.t): CellEditor.Model.t => {
      CellEditor.Model.mk(item.editor);
    };
    let cells =
      CodeExercise.stitch_term(editors)
      |> CodeExercise.map_stitched(_ => term_item_to_cell);
    {
      spec,
      editors,
      cells,
      editing_flags: editing_flags_false,
    };
  };

  /* Editors whose problems appear in the Problems sidebar.
     `test_validation` precedes `user_tests` so first-wins dedup in
     `ProblemCollection.make` puts shared ids in "Test Validation",
     matching jump-to-tile routing. */
  let get_problem_editors =
      (~instructor_mode: bool, model: t)
      : list((option(string), list(CodeEditable.Model.t))) => {
    let c = model.cells;
    let hidden_bug_labels =
      List.mapi(
        (i, b) =>
          (
            CodeExercise.HiddenBugs(i),
            "Mutant " ++ string_of_int(i + 1),
            b,
          ),
        c.hidden_bugs,
      );
    let pairs = [
      (CodeExercise.Prelude, "Prelude", c.prelude),
      (
        CodeExercise.YourTestsValidation,
        "Test Validation",
        c.test_validation,
      ),
      (
        CodeExercise.YourTestsTesting,
        "Implementation Validation",
        c.user_tests,
      ),
      (CodeExercise.YourImpl, "Your Implementation", c.user_impl),
      (CodeExercise.CorrectImpl, "Correct Implementation", c.instructor),
      (CodeExercise.HiddenTests, "Hidden Tests", c.hidden_tests),
      ...hidden_bug_labels,
    ];
    List.filter_map(
      ((pos, label, cell: CellEditor.Model.t)) =>
        CodeExercise.shown_in(pos, ~instructor_mode)
          ? Some((Some(label), [cell.editor])) : None,
      pairs,
    );
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type instructor =
    | EditingTitle
    | EditingPrompt
    | EditingTestValRep
    | EditingMutTestRep
    | EditingImplGrdRep
    | EditingModuleName
    | EditingSyntaxRep
    | UpdateTitle(string)
    | AddBuggyImplementation
    | DeleteBuggyImplementation(int)
    | UpdatePrompt(string)
    | UpdateTestValRep(int, int)
    | UpdateMutTestRep(int, list(string))
    | UpdateImplGrdRep(int, list(string))
    | UpdateSyntaxRep(list(string))
    | UpdateModuleName(string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Editor(CodeExercise.pos, CellEditor.Update.t)
    | RefreshStatics
    | ResetEditor(CodeExercise.pos)
    | ResetExercise
    | Instructor(instructor);

  let instructor_update =
      (action: instructor, model: Model.t): Updated.t(Model.t) =>
    switch (action) {
    // TODO: replace Update.return when appropriate
    | EditingTitle =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_title: !model.editing_flags.editing_title,
        },
      })
    | EditingPrompt =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_prompt: !model.editing_flags.editing_prompt,
        },
      })
    | EditingTestValRep =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_test_val_rep: !model.editing_flags.editing_test_val_rep,
        },
      })
    | EditingMutTestRep =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_mut_test_rep: !model.editing_flags.editing_mut_test_rep,
        },
      })
    | EditingImplGrdRep =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_impl_grd_rep: !model.editing_flags.editing_impl_grd_rep,
        },
      })
    | EditingModuleName =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_module_name: !model.editing_flags.editing_module_name,
        },
      })
    | EditingSyntaxRep =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_syntax_rep: !model.editing_flags.editing_syntax_rep,
        },
      })
    | UpdateTitle(title) =>
      Updated.return_quiet(
        {
          ...model,
          editors:
            CodeExercise.update_exercise_title({eds: model.editors}, title).
              eds,
        },
        ~is_edit=true,
      )
    | AddBuggyImplementation =>
      Updated.return({
        ...model,
        editors: CodeExercise.add_buggy_impl({eds: model.editors}).eds,
        cells: {
          ...model.cells,
          hidden_bugs:
            model.cells.hidden_bugs
            @ [
              CellEditor.Model.mk(Editor.Model.mk(Zipper.init(), ~root=Exp)),
            ],
        },
      })
    | DeleteBuggyImplementation(i) =>
      Updated.return({
        ...model,
        editors: CodeExercise.delete_buggy_impl({eds: model.editors}, i).eds,
        cells: {
          ...model.cells,
          hidden_bugs:
            List.filteri((j, _) => j != i, model.cells.hidden_bugs),
        },
      })
    | UpdatePrompt(prompt) =>
      Updated.return({
        ...model,
        editors:
          CodeExercise.update_exercise_prompt({eds: model.editors}, prompt).
            eds,
      })
    | UpdateTestValRep(test_num, dist) =>
      Updated.return({
        ...model,
        editors:
          CodeExercise.update_test_val_rep(
            {eds: model.editors},
            test_num,
            dist,
          ).
            eds,
      })
    | UpdateMutTestRep(test_num, new_hints) =>
      Updated.return({
        ...model,
        editors:
          CodeExercise.update_mut_test_rep(
            {eds: model.editors},
            test_num,
            new_hints,
          ).
            eds,
      })
    | UpdateSyntaxRep(new_hints) =>
      Updated.return({
        ...model,
        editors:
          CodeExercise.update_syntax_rep({eds: model.editors}, new_hints).
            eds,
      })
    | UpdateImplGrdRep(test_num, new_hints) =>
      Updated.return({
        ...model,
        editors:
          CodeExercise.update_impl_grd_rep(
            {eds: model.editors},
            test_num,
            new_hints,
          ).
            eds,
      })
    | UpdateModuleName(module_name) =>
      Updated.return({
        ...model,
        editors:
          CodeExercise.update_module_name({eds: model.editors}, module_name).
            eds,
      })
    };

  let instructor_update =
      (~settings: Settings.t, action: instructor, model: Model.t)
      : Updated.t(Model.t) =>
    if (settings.instructor_mode) {
      instructor_update(action, model);
    } else {
      Updated.return_quiet(model);
    };

  let update =
      (~settings: Settings.t, ~schedule_action as _, action, model: Model.t)
      : Updated.t(Model.t) => {
    let instructor_mode = settings.instructor_mode;
    switch (action) {
    | Editor(pos, MainEditor(action))
        when CodeExercise.is_editable(pos, ~instructor_mode) =>
      // Redirect to editors
      let editor =
        CodeExercise.main_editor_of_state(~selection=pos, model.editors);
      let cell =
        switch (CodeExercise.get_stitched(pos, model.cells)) {
        | cell_editor => cell_editor
        | exception (Failure(_)) => CellEditor.Model.mk(editor)
        };
      let* new_code_editor =
        // Hack[Matt]: put Editor.t into a CodeEditor.t to use its update function
        {
          ...cell.editor,
          editor,
        }
        |> CodeEditable.Update.update(~settings, action);
      {
        ...model,
        editors:
          CodeExercise.put_main_editor(
            ~selection=pos,
            model.editors,
            new_code_editor.editor,
          ),
        cells:
          CodeExercise.put_stitched(
            pos,
            model.cells,
            {
              ...cell,
              editor: new_code_editor,
            },
          ),
      };
    | Editor(pos, MainEditor(action)) =>
      switch (CodeSelectable.Update.convert_action(action)) {
      | Some(action) =>
        let editor =
          CodeExercise.main_editor_of_state(~selection=pos, model.editors);
        let* new_editor =
          // Hack[Matt]: put Editor.t into a CodeSelectable.t to use its update function
          editor
          |> CodeSelectable.Model.mk
          |> CodeSelectable.Update.update(~settings, action);
        {
          ...model,
          editors:
            CodeExercise.put_main_editor(
              ~selection=pos,
              model.editors,
              new_editor.editor,
            ),
        };
      | None => Updated.return_quiet(model)
      }
    | Editor(pos, ResultAction(_) as action)
        when
          CodeExercise.is_editable(pos, ~instructor_mode)
          || action
          |> (
            fun
            | ResultAction(UpdateResult(_)) => true
            | _ => false
          ) =>
      let cell = CodeExercise.get_stitched(pos, model.cells);
      let* new_cell = CellEditor.Update.update(~settings, action, cell);
      {
        ...model,
        cells: CodeExercise.put_stitched(pos, model.cells, new_cell),
      };
    | Editor(_, ResultAction(_)) => Updated.raise_invalid_action(model) // TODO: I think this case should never happen
    | RefreshStatics =>
      CodeWithStatics.StaticsDebounce.force_on_next := true;
      model |> Updated.return_quiet(~recalculate=true);
    | ResetEditor(pos) =>
      let spec =
        CodeExercise.main_editor_of_state(~selection=pos, model.spec);
      let new_editor = Editor.Model.mk(spec, ~root=Exp);
      {
        ...model,
        editors:
          CodeExercise.put_main_editor(
            ~selection=pos,
            model.editors,
            new_editor,
          ),
      }
      |> Updated.return;
    | ResetExercise =>
      let new_editors =
        CodeExercise.map(
          model.spec,
          Editor.Model.mk(~root=Exp),
          Editor.Model.mk(~root=Exp),
        );
      {
        ...model,
        editors: new_editors,
      }
      |> Updated.return;
    | Instructor(action) => instructor_update(~settings, action, model)
    };
  };

  let calculate =
      (~settings, ~is_edited, ~schedule_action, model: Model.t): Model.t => {
    let statics_mode =
      CodeWithStatics.StaticsDebounce.consume(~is_edited, ~schedule_refresh=() =>
        schedule_action(RefreshStatics)
      );

    let stitched_elabs = CodeExercise.stitch_term(model.editors);
    let worker_request = ref([]);
    let queue_worker = (pos, req_value: WorkerServer.Request.value) => {
      worker_request :=
        worker_request^ @ [(pos |> CodeExercise.key_for_statics, req_value)];
    };
    let cells =
      CodeExercise.map2_stitched(
        (
          pos,
          {term, editor}: CodeExercise.TermItem.t,
          cell: CellEditor.Model.t,
        ) =>
          {
            editor: {
              editor,
              statics: cell.editor.statics,
              dynamics: EvalResult.Model.dynamics(cell.result),
              context_menu: cell.editor.context_menu,
            },
            result: cell.result,
          }
          |> CellEditor.Update.calculate(
               ~settings,
               ~is_edited,
               ~statics_mode,
               ~queue_worker=Some(queue_worker(pos)),
               ~stitch=_ =>
               term
             ),
        stitched_elabs,
        model.cells,
      );

    EvalRequest.request(
      worker_request^,
      ~pos_of_key=CodeExercise.pos_of_key,
      ~dispatch=
        (pos, action) =>
          schedule_action(Editor(pos, ResultAction(action))),
      ~on_timeout=
        _ =>
          ignore(
            CodeExercise.map_stitched(
              (pos, _) =>
                schedule_action(
                  Editor(
                    pos,
                    ResultAction(UpdateResult(ResultFail(Timeout))),
                  ),
                ),
              model.cells,
            ),
          ),
    );

    /* The following section pulls statics back from cells into the editors
       There are many ad-hoc things about this code, including the fact that
       one of the editors is shown in two cells, so we arbitrarily choose which
       statics to take */
    let editors: CodeExercise.p('a) = {
      let calculate = (statics, dynamics, ed) =>
        Editor.Update.calculate(
          ~settings,
          ~autoprobe_mode=false,
          statics,
          dynamics,
          ~is_edited,
          ed,
        );

      {
        id: model.editors.id,
        title: model.editors.title,
        module_name: model.editors.module_name,
        prompt: model.editors.prompt,
        point_distribution: model.editors.point_distribution,
        prelude:
          calculate(
            cells.prelude.editor.statics,
            cells.prelude.editor.dynamics,
            model.editors.prelude,
          ),
        correct_impl:
          calculate(
            cells.test_validation.editor.statics,
            cells.test_validation.editor.dynamics,
            model.editors.correct_impl,
          ),
        your_tests: {
          tests:
            calculate(
              cells.user_tests.editor.statics,
              cells.user_tests.editor.dynamics,
              model.editors.your_tests.tests,
            ),
          required: model.editors.your_tests.required,
          provided: model.editors.your_tests.provided,
        },
        your_impl:
          calculate(
            cells.user_impl.editor.statics,
            cells.user_impl.editor.dynamics,
            model.editors.your_impl,
          ),
        hidden_bugs:
          List.map2(
            (cell: CellEditor.Model.t, editor: CodeExercise.wrong_impl('a)):
              CodeExercise.wrong_impl('a) =>
              {
                impl:
                  calculate(
                    cell.editor.statics,
                    cell.editor.dynamics,
                    editor.impl,
                  ),
                hint: editor.hint,
              },
            cells.hidden_bugs,
            model.editors.hidden_bugs,
          ),
        hidden_tests: {
          tests:
            calculate(
              cells.hidden_tests.editor.statics,
              cells.hidden_tests.editor.dynamics,
              model.editors.hidden_tests.tests,
            ),
          hints: model.editors.hidden_tests.hints,
        },
        syntax_tests: model.editors.syntax_tests,
      };
    };
    {
      spec: model.spec,
      editors,
      cells,
      editing_flags: model.editing_flags,
    };
  };
};

module Selection = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Cell(CodeExercise.pos, CellEditor.Selection.t)
    | TextBox;

  let get_cursor_info =
      (~inject: Update.t => Ui_effect.t(unit), ~selection, model: Model.t)
      : cursor(Update.t) => {
    switch (selection) {
    | Cell(pos, s) =>
      switch (CodeExercise.get_stitched(pos, model.cells)) {
      | cell_editor =>
        let+ a =
          CellEditor.Selection.get_cursor_info(
            ~inject=a => inject(Editor(pos, a)),
            ~selection=s,
            cell_editor,
          );
        Update.Editor(pos, a);
      | exception (Failure(_)) => empty
      }
    | TextBox => empty
    };
  };

  let jump_to_tile =
      (~settings: Settings.t, id: Id.t, model: Model.t)
      : option((Update.t, t)) => {
    CodeExercise.positioned_editors(model.editors)
    |> List.find_opt(((p, e: Editor.t)) =>
         TermData.root_piece(id, e.syntax.term_data) != None
         && CodeExercise.shown_in(
              p,
              ~instructor_mode=settings.instructor_mode,
            )
       )
    |> Option.map(((pos, _)) =>
         (
           Update.Editor(
             pos,
             MainEditor(Perform(Move(Goal(TileId(id))))),
           ),
           Cell(pos, CellEditor.Selection.MainEditor),
         )
       );
  };
};

module View = {
  type event =
    | MakeActive(Selection.t);

  /* The exercises mode interface for a single exercise. Composed of multiple editors and results. */

  /* This file follows conventions in [docs/ui-architecture.md] */

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => 'b,
        ~inject: Update.t => 'b,
        ~inject_explainthis: ExplainThisUpdate.update => 'b,
        ~selection: option(Selection.t),
        model: Model.t,
      ) => {
    let editing_flags = model.editing_flags;

    let eds = model.editors;
    let {
      test_validation,
      user_impl,
      user_tests,
      prelude,
      instructor,
      hidden_bugs,
      hidden_tests,
    }:
      CodeExercise.stitched('a) =
      model.cells;

    /* While a cell is ResultPending, dynamics can reflect a partial stream
       collector — do not treat those as settled grades (same gate as
       TutorialMode's ResultPending checkmark). */
    let settled_test_results =
        (cell_editor: CellEditor.Model.t): option(Language.TestResults.t) =>
      EvalResult.Model.eval_is_pending(cell_editor.result)
        ? None : EvalResult.Model.test_results(cell_editor.result);

    let stitched_tests =
      CodeExercise.map_stitched(
        (_, cell_editor) => settled_test_results(cell_editor),
        model.cells,
      );

    let grading_report = CodeGrading.GradingReport.mk(eds, ~stitched_tests);

    let grading_pending =
      EvalResult.Model.eval_is_pending(test_validation.result)
      || EvalResult.Model.eval_is_pending(hidden_tests.result)
      || List.exists(
           (cell: CellEditor.Model.t) =>
             EvalResult.Model.eval_is_pending(cell.result),
           hidden_bugs,
         );

    let score_view =
      grading_pending
        ? Grading.pending_score_view()
        : CodeGrading.GradingReport.view_overall_score(grading_report);

    /* Renders a cell only if `shown_in`; returns [] when hidden.
       `result_kind` is thunked so hidden cells build nothing. */
    let editor_view =
        (
          ~caption: string,
          ~subcaption: option(string)=?,
          ~result_kind=() => `NoResults,
          this_pos: CodeExercise.pos,
          cell: CellEditor.Model.t,
        )
        : list(Node.t) =>
      CodeExercise.shown_in(
        this_pos,
        ~instructor_mode=globals.settings.instructor_mode,
      )
        ? [
          CellEditor.View.view(
            ~globals,
            ~signal=
              fun
              | MakeActive(a) => signal(MakeActive(Cell(this_pos, a))),
            ~selected=
              switch (selection) {
              | Some(Cell(pos, s)) when pos == this_pos => Some(s)
              | _ => None
              },
            ~inject=a => inject(Editor(this_pos, a)),
            ~result_kind=result_kind(),
            ~caption=
              switch (this_pos) {
              | HiddenBugs(n) =>
                CellCommon.wrong_impl_caption(
                  ~inject_delete=
                    i => inject(Instructor(DeleteBuggyImplementation(i))),
                  caption,
                  n,
                )
              | _ => CellCommon.caption(caption, ~rest=?subcaption)
              },
            ~lines=true,
            cell,
          ),
        ]
        : [];

    let on_focus_textbox = _ => signal(MakeActive(TextBox));

    let title_view =
      InstructorEditViews.title_view(
        ~instructor_mode=globals.settings.instructor_mode,
        ~is_editing=editing_flags.editing_title,
        ~title=eds.title,
        ~on_focus_textbox,
        ~toggle_editing=_ => inject(Instructor(EditingTitle)),
        ~update_title=t => inject(Instructor(UpdateTitle(t))),
      );

    let module_name_view =
      InstructorEditViews.module_name_view(
        ~instructor_mode=globals.settings.instructor_mode,
        ~is_editing=editing_flags.editing_module_name,
        ~module_name=eds.module_name,
        ~on_focus_textbox,
        ~toggle_editing=_ => inject(Instructor(EditingModuleName)),
        ~update_module_name=m => inject(Instructor(UpdateModuleName(m))),
      );

    let prompt_view =
      InstructorEditViews.prompt_view(
        ~globals,
        ~inject_explainthis,
        ~instructor_mode=globals.settings.instructor_mode,
        ~is_editing=editing_flags.editing_prompt,
        ~prompt=eds.prompt,
        ~on_focus_textbox,
        ~toggle_editing=_ => inject(Instructor(EditingPrompt)),
        ~update_prompt=p => inject(Instructor(UpdatePrompt(p))),
      );

    let prelude_view =
      editor_view(
        Prelude,
        prelude,
        ~subcaption=globals.settings.instructor_mode ? "" : " (Read-Only)",
        ~caption="Prelude",
      );

    let correct_impl_view =
      editor_view(CorrectImpl, instructor, ~caption="Correct Implementation");

    // determine trailing hole
    // TODO: module
    let correct_impl_ctx_view = {
      let exp_ctx_view = {
        let correct_impl_trailing_hole_ctx =
          Haz3lcore.Editor.Model.trailing_hole_ctx(
            eds.correct_impl,
            instructor.editor.statics.info_map,
          );
        let prelude_trailing_hole_ctx =
          Haz3lcore.Editor.Model.trailing_hole_ctx(
            eds.prelude,
            prelude.editor.statics.info_map,
          );
        switch (correct_impl_trailing_hole_ctx, prelude_trailing_hole_ctx) {
        | (None, _) => Node.div([text("No context available (1)")])
        | (_, None) => Node.div([text("No context available (2)")]) // TODO show exercise configuration error
        | (
            Some(correct_impl_trailing_hole_ctx),
            Some(prelude_trailing_hole_ctx),
          ) =>
          let specific_ctx =
            Language.Ctx.subtract_prefix(
              correct_impl_trailing_hole_ctx,
              prelude_trailing_hole_ctx,
            );
          switch (specific_ctx) {
          | None => Node.div([text("No context available")]) // TODO show exercise configuration error
          | Some(specific_ctx) =>
            ContextInspector.ctx_view(~globals, specific_ctx)
          };
        };
      };
      CellCommon.simple_cell_view([
        CellCommon.simple_cell_item([
          CellCommon.caption(
            "Correct Implementation",
            ~rest=" (Type Signatures Only)",
          ),
          exp_ctx_view,
        ]),
      ]);
    };

    let rm_probe_data = (editor: CellEditor.Model.t): CellEditor.Model.t => {
      editor: {
        editor: editor.editor.editor,
        statics: editor.editor.statics,
        dynamics: Language.Dynamics.Map.empty,
        context_menu: editor.editor.context_menu,
      },
      result: editor.result,
    };

    let your_tests_view = {
      let subcaption =
        globals.settings.instructor_mode
          ? ": Student Tests vs. Correct Implementation"
          : ": Your Tests vs. Correct Implementation";
      editor_view(
        YourTestsValidation,
        // Remove probe data from this cell to prevent data leaks from correct implementation
        rm_probe_data(test_validation),
        ~caption="Test Validation",
        ~subcaption,
        ~result_kind=() =>
        `Custom(
          CodeGrading.TestValidationReport.view(
            ~globals,
            ~signal_jump=
              id =>
                inject(
                  Editor(
                    YourTestsValidation,
                    MainEditor(Perform(Move(Goal(TileId(id))))),
                  ),
                ),
            ~signal_editing_test_val_rep=
              inject(Instructor(EditingTestValRep)),
            ~signal_update_test_val=
              (x, y) => inject(Instructor(UpdateTestValRep(x, y))),
            ~signal_textbox_active=signal(MakeActive(TextBox)),
            ~editing_test_val_rep=editing_flags.editing_test_val_rep,
            ~eval_pending=
              EvalResult.Model.eval_is_pending(test_validation.result),
            grading_report.test_validation_report,
            grading_report.point_distribution.test_validation,
            eds.your_tests.required,
          ),
        )
      );
    };

    let mutation_testing_view =
      CodeGrading.MutationTestingReport.view(
        ~globals,
        ~editing_mut_test_rep=editing_flags.editing_mut_test_rep,
        ~inject_editing_mut_test_rep=inject(Instructor(EditingMutTestRep)),
        ~inject_update_mut_test_rep=
          (x, y) => inject(Instructor(UpdateMutTestRep(x, y))),
        ~select_textbox=signal(MakeActive(TextBox)),
        ~eval_pending=
          EvalResult.Model.eval_is_pending(test_validation.result)
          || List.exists(
               (cell: CellEditor.Model.t) =>
                 EvalResult.Model.eval_is_pending(cell.result),
               hidden_bugs,
             ),
        grading_report.mutation_testing_report,
        grading_report.point_distribution.mutation_testing,
      );

    let your_impl_view = {
      let caption =
        globals.settings.instructor_mode
          ? "Student's Implementation" : "Your Implementation";
      editor_view(YourImpl, user_impl, ~caption, ~result_kind=() =>
        `EvalResults
      );
    };

    let syntax_grading_view =
      CodeGrading.SyntaxReport.view(
        ~globals,
        ~editing_syntax_rep=editing_flags.editing_syntax_rep,
        ~inject_set_editing_syntax_rep=inject(Instructor(EditingSyntaxRep)),
        ~inject_update_syntax_rep=
          hints => inject(Instructor(UpdateSyntaxRep(hints))),
        ~select_textbox=signal(MakeActive(TextBox)),
        grading_report.syntax_report,
      );

    let impl_validation_view = {
      let subcaption =
        globals.settings.instructor_mode
          ? ": Student's Tests vs. Student's Implementation"
          : ": Your Tests (code synchronized with Test Validation cell above) vs. Your Implementation";
      editor_view(
        YourTestsTesting,
        user_tests,
        ~caption="Implementation Validation",
        ~subcaption,
        ~result_kind=() =>
        `TestResults
      );
    };

    let hidden_tests_view =
      editor_view(HiddenTests, hidden_tests, ~caption="Hidden Tests");

    let impl_grading_view =
      CodeGrading.ImplGradingReport.view(
        ~globals,
        ~signal_jump=
          id =>
            inject(
              Editor(
                YourTestsTesting,
                MainEditor(Perform(Move(Goal(TileId(id))))),
              ),
            ),
        ~inject_set_editing_impl_grd_rep=
          inject(Instructor(EditingImplGrdRep)),
        ~inject_update_impl_grd_rep=
          (x, y) => inject(Instructor(UpdateImplGrdRep(x, y))),
        ~select_textbox=signal(MakeActive(TextBox)),
        ~editing_impl_grd_rep=editing_flags.editing_impl_grd_rep,
        ~eval_pending=EvalResult.Model.eval_is_pending(hidden_tests.result),
        ~report=grading_report.impl_grading_report,
        ~syntax_report=grading_report.syntax_report,
        ~max_points=grading_report.point_distribution.impl_grading,
      );

    /* Instructor-only authoring section; thunked so students build none of it. */
    let wrong_impl_section = () => {
      let mutant_views =
        List.combine(eds.hidden_bugs, hidden_bugs)
        |> List.mapi((i, (_, cell)) =>
             editor_view(
               HiddenBugs(i),
               cell,
               ~caption="Mutant " ++ string_of_int(i + 1),
             )
           )
        |> List.concat;
      let add_view =
        CellCommon.simple_cell_view([
          CellCommon.simple_cell_item([
            div(
              ~attrs=[Attr.class_("wrong-impl-cell-caption")],
              [
                div(
                  ~attrs=[
                    Attr.class_("instructor-edit-icon"),
                    Attr.id("add-icon"),
                  ],
                  [
                    Widgets.button(
                      Icons.add,
                      _ =>
                        Ui_effect.Many([
                          inject(Instructor(AddBuggyImplementation)),
                          signal(
                            MakeActive(
                              Cell(
                                HiddenBugs(List.length(hidden_bugs)),
                                MainEditor,
                              ),
                            ),
                          ),
                        ]),
                      ~tooltip="Add Buggy Implementation",
                    ),
                  ],
                ),
              ],
            ),
          ]),
        ]);
      CellCommon.simple_cell_view([
        CellCommon.simple_cell_item(
          [CellCommon.caption("Mutation Tests")] @ mutant_views @ [add_view],
        ),
      ]);
    };

    /* Editor cells self-gate (see `editor_view`); non-editor views always
       show; the mutants section is instructor-only. */
    [score_view, title_view, module_name_view, prompt_view]
    @ prelude_view
    @ correct_impl_view
    @ [correct_impl_ctx_view]
    @ your_tests_view
    @ (globals.settings.instructor_mode ? [wrong_impl_section()] : [])
    @ [mutation_testing_view]
    @ your_impl_view
    @ [syntax_grading_view]
    @ impl_validation_view
    @ hidden_tests_view
    @ [impl_grading_view];
  };
};
