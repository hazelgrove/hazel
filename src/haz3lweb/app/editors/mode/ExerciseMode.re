open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

/* The exercises mode interface for a single exercise. Composed of multiple editors and results. */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    spec: Exercise.spec, // The spec that the model will be reset to on ResetExercise
    /* We keep a separate editors field below (even though each cell technically also has its own editor)
       for two reasons:
          1. There are two synced cells that have the same internal `editor` model
          2. The editors need to be `stitched` together before any cell calculations can be done */
    editors: Exercise.p(Editor.t),
    cells: Exercise.stitched(CellEditor.Model.t),
  };

  let of_spec = (~settings as _, ~instructor_mode as _: bool, spec) => {
    let editors = Exercise.map(spec, Editor.Model.mk, Editor.Model.mk);
    let term_item_to_cell = (item: Exercise.TermItem.t): CellEditor.Model.t => {
      CellEditor.Model.mk(item.editor);
    };
    let cells =
      Exercise.stitch_term(editors)
      |> Exercise.map_stitched(_ => term_item_to_cell);
    {spec, editors, cells};
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = Exercise.persistent_exercise_mode;

  let persist = (exercise: t, ~instructor_mode: bool) => {
    Exercise.positioned_editors(exercise.editors)
    |> List.filter(((pos, _)) =>
         Exercise.visible_in(pos, ~instructor_mode)
       )
    |> List.map(((pos, editor: Editor.t)) =>
         (pos, editor.state.zipper |> PersistentZipper.persist)
       );
  };

  let unpersist = (~instructor_mode, positioned_zippers, spec) => {
    let spec = Exercise.unpersist(~instructor_mode, positioned_zippers, spec);
    of_spec(~instructor_mode, spec);
  };
};

module Update = {
  open Updated;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Editor(Exercise.pos, CellEditor.Update.t)
    | ResetEditor(Exercise.pos)
    | ResetExercise;

  let update =
      (~settings: Settings.t, ~schedule_action as _, action, model: Model.t)
      : Updated.t(Model.t) => {
    let instructor_mode = settings.instructor_mode;
    switch (action) {
    | Editor(pos, MainEditor(action))
        when Exercise.visible_in(pos, ~instructor_mode) =>
      // Redirect to editors
      let editor =
        Exercise.main_editor_of_state(~selection=pos, model.editors);
      let* new_editor =
        // Hack[Matt]: put Editor.t into a CodeEditor.t to use its update function
        editor
        |> CodeEditable.Model.mk
        |> CodeEditable.Update.update(~settings, action);
      {
        ...model,
        editors:
          Exercise.put_main_editor(
            ~selection=pos,
            model.editors,
            new_editor.editor,
          ),
      };
    | Editor(pos, MainEditor(action)) =>
      switch (CodeSelectable.Update.convert_action(action)) {
      | Some(action) =>
        let editor =
          Exercise.main_editor_of_state(~selection=pos, model.editors);
        let* new_editor =
          // Hack[Matt]: put Editor.t into a CodeSelectable.t to use its update function
          editor
          |> CodeSelectable.Model.mk
          |> CodeSelectable.Update.update(~settings, action);
        {
          ...model,
          editors:
            Exercise.put_main_editor(
              ~selection=pos,
              model.editors,
              new_editor.editor,
            ),
        };
      | None => Updated.return_quiet(model)
      }
    | Editor(pos, ResultAction(_) as action)
        when
          Exercise.visible_in(pos, ~instructor_mode)
          || action
          |> (
            fun
            | ResultAction(UpdateResult(_)) => true
            | _ => false
          ) =>
      let cell = Exercise.get_stitched(pos, model.cells);
      let* new_cell = CellEditor.Update.update(~settings, action, cell);
      {...model, cells: Exercise.put_stitched(pos, model.cells, new_cell)};
    | Editor(_, ResultAction(_)) => Updated.return_quiet(model) // TODO: I think this case should never happen
    | ResetEditor(pos) =>
      let spec = Exercise.main_editor_of_state(~selection=pos, model.spec);
      let new_editor = Editor.Model.mk(spec);
      {
        ...model,
        editors:
          Exercise.put_main_editor(~selection=pos, model.editors, new_editor),
      }
      |> Updated.return;
    | ResetExercise =>
      let new_editors =
        Exercise.map(model.spec, Editor.Model.mk, Editor.Model.mk);
      {...model, editors: new_editors} |> Updated.return;
    };
  };

  let calculate =
      (~settings, ~is_edited, ~schedule_action, model: Model.t): Model.t => {
    let stitched_elabs = Exercise.stitch_term(model.editors);
    let worker_request = ref([]);
    let queue_worker = (pos, expr) => {
      worker_request :=
        worker_request^ @ [(pos |> Exercise.key_for_statics, expr)];
    };
    let cells =
      Exercise.map2_stitched(
        (pos, {term, editor}: Exercise.TermItem.t, cell: CellEditor.Model.t) =>
          {
            editor: {
              editor,
              statics: cell.editor.statics,
            },
            result: cell.result,
          }
          |> CellEditor.Update.calculate(
               ~settings,
               ~is_edited,
               ~queue_worker=Some(queue_worker(pos)),
               ~stitch=_ =>
               term
             ),
        stitched_elabs,
        model.cells,
      );
    WorkerClient.request(
      worker_request^,
      ~handler=
        List.iter(((pos, result)) => {
          let pos' = Exercise.pos_of_key(pos);
          let result':
            Haz3lcore.ProgramResult.t(Haz3lcore.ProgramResult.inner) =
            switch (result) {
            | Ok((r, s)) => ResultOk({result: r, state: s})
            | Error(e) => ResultFail(e)
            };
          schedule_action(
            Editor(pos', ResultAction(UpdateResult(result'))),
          );
        }),
      ~timeout=_ => {
        let _ =
          Exercise.map_stitched(
            (pos, _) =>
              schedule_action(
                Editor(
                  pos,
                  ResultAction(UpdateResult(ResultFail(Timeout))),
                ),
              ),
            model.cells,
          );
        ();
      },
    );
    {spec: model.spec, editors: model.editors, cells};
  };
};

module Selection = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Exercise.pos, CellEditor.Selection.t);

  let get_cursor_info = (~selection, model: Model.t): cursor(Update.t) => {
    let (pos, s) = selection;
    let cell_editor = Exercise.get_stitched(pos, model.cells);
    let+ a = CellEditor.Selection.get_cursor_info(~selection=s, cell_editor);
    Update.Editor(pos, a);
  };

  let handle_key_event = (~selection, ~event, model: Model.t) => {
    let (pos, s) = selection;
    let cell_editor = Exercise.get_stitched(pos, model.cells);
    CellEditor.Selection.handle_key_event(~selection=s, ~event, cell_editor)
    |> Option.map(a => Update.Editor(pos, a));
  };

  let jump_to_tile =
      (~settings: Settings.t, tile, model: Model.t): option((Update.t, t)) => {
    Exercise.positioned_editors(model.editors)
    |> List.find_opt(((p, e: Editor.t)) =>
         TileMap.find_opt(tile, e.syntax.tiles) != None
         && Exercise.visible_in(p, ~instructor_mode=settings.instructor_mode)
       )
    |> Option.map(((pos, _)) =>
         (
           Update.Editor(pos, MainEditor(Perform(Jump(TileId(tile))))),
           (pos, CellEditor.Selection.MainEditor),
         )
       );
  };
};

module View = {
  type event =
    | MakeActive(Selection.t);

  type vis_marked('a) =
    | InstructorOnly(unit => 'a)
    | Always('a);

  let render_cells = (settings: Settings.t, v: list(vis_marked(Node.t))) => {
    List.filter_map(
      vis =>
        switch (vis) {
        | InstructorOnly(f) => settings.instructor_mode ? Some(f()) : None
        | Always(node) => Some(node)
        },
      v,
    );
  };

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => 'b,
        ~inject: Update.t => 'b,
        ~selection: option(Selection.t),
        model: Model.t,
      ) => {
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
      Exercise.stitched('a) =
      model.cells;

    let stitched_tests =
      Exercise.map_stitched(
        (_, cell_editor: CellEditor.Model.t) =>
          cell_editor.result |> EvalResult.Model.make_test_report,
        model.cells,
      );

    let grading_report = Grading.GradingReport.mk(eds, ~stitched_tests);

    let score_view = Grading.GradingReport.view_overall_score(grading_report);

    let editor_view =
        (
          ~caption: string,
          ~subcaption: option(string)=?,
          ~result_kind=EvalResult.View.NoResults,
          this_pos: Exercise.pos,
          cell: CellEditor.Model.t,
        ) => {
      CellEditor.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(a) => signal(MakeActive((this_pos, a))),
        ~selected=
          switch (selection) {
          | Some((pos, s)) when pos == this_pos => Some(s)
          | _ => None
          },
        ~inject=a => inject(Editor(this_pos, a)),
        ~result_kind,
        ~caption=CellCommon.caption(caption, ~rest=?subcaption),
        cell,
      );
    };

    let title_view = CellCommon.title_cell(eds.title);

    let prompt_view =
      CellCommon.narrative_cell(
        div(~attrs=[Attr.class_("cell-prompt")], [eds.prompt]),
      );

    let prelude_view =
      Always(
        editor_view(
          Prelude,
          prelude,
          ~subcaption=globals.settings.instructor_mode ? "" : " (Read-Only)",
          ~caption="Prelude",
        ),
      );

    let correct_impl_view =
      InstructorOnly(
        () =>
          editor_view(
            CorrectImpl,
            instructor,
            ~caption="Correct Implementation",
          ),
      );

    // determine trailing hole
    // TODO: module
    let correct_impl_ctx_view =
      Always(
        {
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
                Haz3lcore.Ctx.subtract_prefix(
                  correct_impl_trailing_hole_ctx,
                  prelude_trailing_hole_ctx,
                );
              switch (specific_ctx) {
              | None => Node.div([text("No context available")]) // TODO show exercise configuration error
              | Some(specific_ctx) =>
                CtxInspector.ctx_view(~globals, specific_ctx)
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
        },
      );

    let your_tests_view =
      Always(
        editor_view(
          YourTestsValidation,
          test_validation,
          ~caption="Test Validation",
          ~subcaption=": Your Tests vs. Correct Implementation",
          ~result_kind=TestResults,
        ),
      );

    let wrong_impl_views =
      List.mapi(
        (i, cell) => {
          InstructorOnly(
            () =>
              editor_view(
                HiddenBugs(i),
                cell,
                ~caption="Wrong Implementation " ++ string_of_int(i + 1),
              ),
          )
        },
        hidden_bugs,
      );

    let mutation_testing_view =
      Always(
        Grading.MutationTestingReport.view(
          ~inject,
          grading_report.mutation_testing_report,
          grading_report.point_distribution.mutation_testing,
        ),
      );

    let your_impl_view = {
      Always(
        editor_view(
          YourImpl,
          user_impl,
          ~caption="Your Implementation",
          ~result_kind=EvalResults,
        ),
      );
    };

    let syntax_grading_view =
      Always(Grading.SyntaxReport.view(grading_report.syntax_report));

    let impl_validation_view =
      Always(
        editor_view(
          YourTestsTesting,
          user_tests,
          ~caption="Implementation Validation",
          ~subcaption=
            ": Your Tests (code synchronized with Test Validation cell above) vs. Your Implementation",
          ~result_kind=TestResults,
        ),
      );

    let hidden_tests_view =
      InstructorOnly(
        () => editor_view(HiddenTests, hidden_tests, ~caption="Hidden Tests"),
      );

    let impl_grading_view =
      Always(
        Grading.ImplGradingReport.view(
          ~signal_jump=
            id =>
              inject(
                Editor(
                  YourTestsTesting,
                  MainEditor(Perform(Jump(TileId(id)))),
                ),
              ),
          ~report=grading_report.impl_grading_report,
          ~syntax_report=grading_report.syntax_report,
          ~max_points=grading_report.point_distribution.impl_grading,
        ),
      );

    [score_view, title_view, prompt_view]
    @ render_cells(
        globals.settings,
        [
          prelude_view,
          correct_impl_view,
          correct_impl_ctx_view,
          your_tests_view,
        ]
        @ wrong_impl_views
        @ [
          mutation_testing_view,
          your_impl_view,
          syntax_grading_view,
          impl_validation_view,
          hidden_tests_view,
          impl_grading_view,
        ],
      );
  };
};
