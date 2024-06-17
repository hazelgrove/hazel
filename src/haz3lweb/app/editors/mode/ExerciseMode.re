open Haz3lcore;
open Virtual_dom.Vdom;
open Sexplib.Std;
open Node;

/* The exercises mode interface for a single exercise. Composed of multiple editors and results. */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    spec: Exercise.spec,
    editors: Exercise.p(Editor.t),
    cells: Exercise.stitched(CellEditor.Model.t),
  };

  let of_spec = (~instructor_mode: bool, spec) => {
    let editors =
      Exercise.map(
        spec,
        Editor.init,
        Editor.init(~read_only=!instructor_mode),
      );
    let term_item_to_cell = (item: Exercise.TermItem.t): CellEditor.Model.t => {
      CellEditor.Model.mk(item.editor);
    };
    let cells =
      Exercise.stitch_term(editors)
      |> Exercise.map_stitched(_ => term_item_to_cell);
    {spec, editors, cells};
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = list((Exercise.pos, PersistentZipper.t));

  let persist = (exercise: t, ~instructor_mode: bool) => {
    Exercise.positioned_editors(exercise.editors)
    |> List.filter(((pos, _)) =>
         Exercise.visible_in(pos, ~instructor_mode)
       )
    |> List.map(((pos, editor: Editor.t)) =>
         (pos, editor.state.zipper |> PersistentZipper.persist)
       );
  };

  let unpersist =
      (~instructor_mode, positioned_zippers, spec: Exercise.spec): t => {
    open Exercise;
    let lookup = (pos, default) =>
      if (visible_in(pos, ~instructor_mode)) {
        positioned_zippers |> List.assoc(pos) |> PersistentZipper.unpersist;
      } else {
        default |> PersistentZipper.persist |> PersistentZipper.unpersist;
      };
    let prelude = lookup(Prelude, spec.prelude);
    let correct_impl = lookup(CorrectImpl, spec.correct_impl);
    let your_tests_tests = lookup(YourTestsValidation, spec.your_tests.tests);
    let your_impl = lookup(YourImpl, spec.your_impl);
    let (_, hidden_bugs) =
      List.fold_left(
        ((i, hidden_bugs: list(wrong_impl('a))), {impl, hint}) => {
          let impl = lookup(HiddenBugs(i), impl);
          (i + 1, hidden_bugs @ [{impl, hint}]);
        },
        (0, []),
        spec.hidden_bugs,
      );
    let hidden_tests_tests = lookup(HiddenTests, spec.hidden_tests.tests);
    {
      title: spec.title,
      version: spec.version,
      module_name: spec.module_name,
      prompt: spec.prompt,
      point_distribution: spec.point_distribution,
      prelude,
      correct_impl,
      your_tests: {
        tests: your_tests_tests,
        required: spec.your_tests.required,
        provided: spec.your_tests.provided,
      },
      your_impl,
      hidden_bugs,
      hidden_tests: {
        tests: hidden_tests_tests,
        hints: spec.hidden_tests.hints,
      },
      syntax_tests: spec.syntax_tests,
    }
    |> of_spec(~instructor_mode);
  };
};

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
    | Editor(pos, ResultAction(_) as action)
        when Exercise.visible_in(pos, ~instructor_mode) =>
      let cell = Exercise.get_stitched(pos, model.cells);
      let* new_cell = CellEditor.Update.update(~settings, action, cell);
      {...model, cells: Exercise.put_stitched(pos, model.cells, new_cell)};
    | Editor(_, MainEditor(_) | ResultAction(_)) =>
      Updated.return_quiet(model) // TODO: better feedback
    | ResetEditor(pos) =>
      let spec = Exercise.main_editor_of_state(~selection=pos, model.spec);
      let new_editor = Editor.init(spec);
      {
        ...model,
        editors:
          Exercise.put_main_editor(~selection=pos, model.editors, new_editor),
      }
      |> Updated.return;
    | ResetExercise =>
      let new_editors =
        Exercise.map(
          model.spec,
          Editor.init,
          Editor.init(~read_only=settings.instructor_mode),
        );
      {...model, editors: new_editors} |> Updated.return;
    };
  };

  let calculate = (~settings, ~schedule_action, model: Model.t): Model.t => {
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
               ~settings, ~queue_worker=Some(queue_worker(pos)), ~stitch=_ =>
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
         TileMap.find_opt(tile, e.state.meta.tiles) != None
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

    let instructor_mode = globals.settings.instructor_mode;

    let stitched_tests =
      Exercise.map_stitched(
        (_, cell_editor: CellEditor.Model.t) =>
          cell_editor.result |> Result.Model.make_test_report,
        model.cells,
      );

    let grading_report = Grading.GradingReport.mk(eds, ~stitched_tests);

    let score_view = Grading.GradingReport.view_overall_score(grading_report);

    let editor_view =
        (
          ~caption: string,
          ~subcaption: option(string)=?,
          ~result_kind=Result.View.NoResults,
          this_pos: Exercise.pos,
          cell: CellEditor.Model.t,
        ) => {
      CellEditor.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(a) =>
            Exercise.visible_in(this_pos, ~instructor_mode)
              ? signal(MakeActive((this_pos, a))) : Effect.Ignore,
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
        div(~attr=Attr.class_("cell-prompt"), [eds.prompt]),
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
              Haz3lcore.Editor.trailing_hole_ctx(
                eds.correct_impl,
                instructor.editor.statics.info_map,
              );
            let prelude_trailing_hole_ctx =
              Haz3lcore.Editor.trailing_hole_ctx(
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
                CtxInspector.ctx_view(
                  ~globals,
                  ~inject=globals.inject_global,
                  specific_ctx,
                )
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

  // NUT MENU ITEMS

  let export_menu = (~globals: Globals.t, model: Model.t) => {
    let download_editor_state = (~instructor_mode) =>
      globals.get_log_and(log => {
        let data = globals.export_all(~instructor_mode, ~log);
        JsUtil.download_json(ExerciseSettings.filename, data);
      });

    let export_submission = () =>
      Widgets.button_named(
        Icons.star,
        _ => {
          download_editor_state(
            ~instructor_mode=globals.settings.instructor_mode,
          );
          Virtual_dom.Vdom.Effect.Ignore;
        },
        ~tooltip="Export Submission",
      );

    let instructor_export = () =>
      Widgets.button_named(
        Icons.star,
        _ => {
          // .ml files because show uses OCaml syntax (dune handles seamlessly)
          let module_name = model.editors.module_name;
          let filename = model.editors.module_name ++ ".ml";
          let content_type = "text/plain";
          let contents =
            Exercise.export_module(module_name, {eds: model.editors});
          JsUtil.download_string_file(~filename, ~content_type, ~contents);
          Virtual_dom.Vdom.Effect.Ignore;
        },
        ~tooltip="Export Exercise Module",
      );

    let instructor_transitionary_export = () =>
      Widgets.button_named(
        Icons.star,
        _ => {
          // .ml files because show uses OCaml syntax (dune handles seamlessly)
          let module_name = model.editors.module_name;
          let filename = model.editors.module_name ++ ".ml";
          let content_type = "text/plain";
          let contents =
            Exercise.export_transitionary_module(
              module_name,
              {eds: model.editors},
            );
          JsUtil.download_string_file(~filename, ~content_type, ~contents);
          Virtual_dom.Vdom.Effect.Ignore;
        },
        ~tooltip="Export Transitionary Exercise Module",
      );

    let instructor_grading_export = () =>
      Widgets.button_named(
        Icons.star,
        _ => {
          // .ml files because show uses OCaml syntax (dune handles seamlessly)
          let module_name = model.editors.module_name;
          let filename = model.editors.module_name ++ "_grading.ml";
          let content_type = "text/plain";
          let contents =
            Exercise.export_grading_module(
              module_name,
              {eds: model.editors},
            );
          JsUtil.download_string_file(~filename, ~content_type, ~contents);
          Virtual_dom.Vdom.Effect.Ignore;
        },
        ~tooltip="Export Grading Exercise Module",
      );

    globals.settings.instructor_mode
      ? [
        export_submission(),
        instructor_export(),
        instructor_transitionary_export(),
        instructor_grading_export(),
      ]
      : [export_submission()];
  };

  let import_menu = (~globals: Globals.t, ~inject: Update.t => 'b) => {
    let import_submission =
      Widgets.file_select_button_named(
        "import-submission",
        Icons.star,
        file => {
          switch (file) {
          | None => Virtual_dom.Vdom.Effect.Ignore
          | Some(file) => globals.inject_global(InitImportAll(file))
          }
        },
        ~tooltip="Import Submission",
      );

    let reset_button =
      Widgets.button_named(
        Icons.trash,
        _ => {
          let confirmed =
            JsUtil.confirm(
              "Are you SURE you want to reset this exercise? You will lose any existing code that you have written, and course staff have no way to restore it!",
            );
          if (confirmed) {
            inject(Update.ResetExercise);
          } else {
            Virtual_dom.Vdom.Effect.Ignore;
          };
        },
        ~tooltip="Reset Exercise",
      );

    [import_submission, reset_button];
  };
};
