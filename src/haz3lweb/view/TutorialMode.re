open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Util;
/* The exercises mode interface for a single exercise. Composed of multiple editors and results. */
/* This file follows conventions in [docs/ui-architecture.md] */
module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    spec: Tutorial.spec, // The spec that the model will be reset to on ResetExercise
    /* We keep a separate editors field below (even though each cell technically also has its own editor)
       for two reasons:
         1. There are two synced cells that have the same internal `editor` model
         2. The editors need to be `stitched` together before any cell calculations can be done */
    editors: Tutorial.p(Editor.t),
    cells: Tutorial.stitched(CellEditor.Model.t),
  };
  let of_spec = (~settings as _, ~instructor_mode as _: bool, spec) => {
    let editors = Tutorial.map(spec, Editor.Model.mk, Editor.Model.mk);
    let term_item_to_cell = (item: Tutorial.TermItem.t): CellEditor.Model.t => {
      CellEditor.Model.mk(item.editor);
    };
    let cells =
      Tutorial.stitch_term(editors)
      |> Tutorial.map_stitched(_ => term_item_to_cell);
    {spec, editors, cells};
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = Tutorial.persistent_exercise_mode;
  let persist = (exercise: t, ~instructor_mode: bool) => {
    Tutorial.positioned_editors(exercise.editors)
    |> List.filter(((pos, _)) =>
         Tutorial.visible_in(pos, ~instructor_mode)
       )
    |> List.map(((pos, editor: Editor.t)) =>
         (pos, editor.state.zipper |> PersistentZipper.persist)
       );
  };
  let unpersist = (~instructor_mode, positioned_zippers, spec) => {
    let spec = Tutorial.unpersist(~instructor_mode, positioned_zippers, spec);
    of_spec(~instructor_mode, spec);
  };
};
module Update = {
  open Updated;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Editor(Tutorial.pos, CellEditor.Update.t)
    | ResetEditor(Tutorial.pos)
    | ResetTutorial
    | Change_report_view;
  let update =
      (~settings: Settings.t, ~schedule_action as _, action, model: Model.t)
      : Updated.t(Model.t) => {
    let instructor_mode = settings.instructor_mode;
    switch (action) {
    | Editor(pos, MainEditor(action))
        when Tutorial.visible_in(pos, ~instructor_mode) =>
      // Redirect to editors
      let editor =
        Tutorial.main_editor_of_state(~selection=pos, model.editors);
      let* new_editor =
        // Hack[Matt]: put Editor.t into a CodeEditor.t to use its update function
        editor
        |> CodeEditable.Model.mk
        |> CodeEditable.Update.update(~settings, action);
      {
        ...model,
        editors:
          Tutorial.put_main_editor(
            ~selection=pos,
            model.editors,
            new_editor.editor,
          ),
      };
    | Editor(pos, MainEditor(action)) =>
      switch (CodeSelectable.Update.convert_action(action)) {
      | Some(action) =>
        let editor =
          Tutorial.main_editor_of_state(~selection=pos, model.editors);
        let* new_editor =
          // Hack[Matt]: put Editor.t into a CodeSelectable.t to use its update function
          editor
          |> CodeSelectable.Model.mk
          |> CodeSelectable.Update.update(~settings, action);
        {
          ...model,
          editors:
            Tutorial.put_main_editor(
              ~selection=pos,
              model.editors,
              new_editor.editor,
            ),
        };
      | None => Updated.return_quiet(model)
      }
    | Editor(pos, ResultAction(_) as action)
        when
          Tutorial.visible_in(pos, ~instructor_mode)
          || action
          |> (
            fun
            | ResultAction(UpdateResult(_)) => true
            | _ => false
          ) =>
      let cell = Tutorial.get_stitched(pos, model.cells);
      let* new_cell = CellEditor.Update.update(~settings, action, cell);
      {...model, cells: Tutorial.put_stitched(pos, model.cells, new_cell)};
    | Editor(_, ResultAction(_)) => Updated.return_quiet(model) // TODO: I think this case should never happen
    | ResetEditor(pos) =>
      let spec = Tutorial.main_editor_of_state(~selection=pos, model.spec);
      let new_editor = Editor.Model.mk(spec);
      {
        ...model,
        editors:
          Tutorial.put_main_editor(~selection=pos, model.editors, new_editor),
      }
      |> Updated.return;
    | ResetTutorial =>
      let new_editors =
        Tutorial.map(model.spec, Editor.Model.mk, Editor.Model.mk);
      {...model, editors: new_editors} |> Updated.return;
    | Change_report_view =>
      Updated.return_quiet({
        ...model,
        editors: {
          ...model.editors,
          show_report: !model.editors.show_report,
        },
      })
    };
  };
  let calculate =
      (~settings, ~is_edited, ~schedule_action, model: Model.t): Model.t => {
    let stitched_elabs = Tutorial.stitch_term(model.editors);
    let worker_request = ref([]);
    let queue_worker = (pos, expr) => {
      worker_request :=
        worker_request^ @ [(pos |> Tutorial.key_for_statics, expr)];
    };
    let cells =
      Tutorial.map2_stitched(
        (pos, {term, editor}: Tutorial.TermItem.t, cell: CellEditor.Model.t) =>
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
          let pos' = Tutorial.pos_of_key(pos);
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
          Tutorial.map_stitched(
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
    /* The following section pulls statics back from cells into the editors
       There are many ad-hoc things about this code, including the fact that
       one of the editors is shown in two cells, so we arbitrarily choose which
       statics to take */
    let editors: Tutorial.p('a) = {
      {
        // let calculate = Editor.Update.calculate(~is_edited);

        title: model.editors.title,
        // description: model.editors.description,
        version: model.editors.version,
        module_name: model.editors.module_name,
        prompt: model.editors.prompt,
        // point_distribution: model.editors.point_distribution,
        // prelude:
        //   calculate(cells.prelude.editor.statics, model.editors.prelude),
        // correct_impl: model.editors.correct_impl,
        // calculate(
        //   // cells.editor.statics,
        //   model.editors.correct_impl,
        // ),
        // your_tests: {
        //   tests:
        //     calculate(
        //       cells.user_tests.editor.statics,
        //       model.editors.your_tests.tests,
        //     ),
        //   required: model.editors.your_tests.required,
        //   provided: model.editors.your_tests.provided,
        // },
        your_impl: model.editors.your_impl,
        // raw_result: model.editors.raw_result,
        // calculate(cells.user_impl.editor.statics, model.editors.your_impl),
        // hidden_bugs:
        //   List.map2(
        //     (cell: CellEditor.Model.t, editor: Exercise.wrong_impl('a)):
        //       Exercise.wrong_impl('a) =>
        //       {
        //         impl: calculate(cell.editor.statics, editor.impl),
        //         hint: editor.hint,
        //       },
        //     cells.hidden_bugs,
        //     model.editors.hidden_bugs,
        //   ),
        hidden_tests: {
          tests: model.editors.hidden_tests.tests,
          // calculate(
          //   cells.hidden_tests.editor.statics,
          //   model.editors.hidden_tests.tests,
          // ),
          hints: model.editors.hidden_tests.hints,
        },
        wrapper: model.editors.wrapper,
        show_report: model.editors.show_report,
        // syntax_tests: model.editors.syntax_tests,
      };
    };
    {spec: model.spec, editors, cells};
  };
};
module Selection = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (Tutorial.pos, CellEditor.Selection.t);
  let get_cursor_info = (~selection, model: Model.t): cursor(Update.t) => {
    let (pos, s) = selection;
    let cell_editor = Tutorial.get_stitched(pos, model.cells);
    let+ a = CellEditor.Selection.get_cursor_info(~selection=s, cell_editor);
    Update.Editor(pos, a);
  };
  let handle_key_event = (~selection, ~event, model: Model.t) => {
    let (pos, s) = selection;
    let cell_editor = Tutorial.get_stitched(pos, model.cells);
    CellEditor.Selection.handle_key_event(~selection=s, ~event, cell_editor)
    |> Option.map(a => Update.Editor(pos, a));
  };
  let jump_to_tile =
      (~settings: Settings.t, tile, model: Model.t): option((Update.t, t)) => {
    Tutorial.positioned_editors(model.editors)
    |> List.find_opt(((p, e: Editor.t)) =>
         TileMap.find_opt(tile, e.syntax.tiles) != None
         && Tutorial.visible_in(p, ~instructor_mode=settings.instructor_mode)
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
    let {user_impl, instructor, hidden_tests}: Tutorial.stitched('a) =
      model.cells;
    // Printf.printf(
    //   "User Impl Before View: %s\n",
    //   CellEditor.Model.show(user_impl),
    // );

    // let answer_value = raw_result.result;

    let stitched_tests =
      Tutorial.map_stitched(
        (_, cell_editor: CellEditor.Model.t) =>
          cell_editor.result |> EvalResult.Model.make_test_report,
        model.cells,
      );
    let test_count =
      switch (Tutorial.get_stitched(HiddenTests, stitched_tests)) {
      | None => 0 /* No test cases */
      | Some(test_results) => test_results.total
      };
    let grading_report =
      TutorialGrading.GradingReport.mk(eds, ~stitched_tests);

    let editor_view =
        (
          ~caption: string,
          ~subcaption: option(string)=?,
          ~result_kind=EvalResult.View.NoResults,
          this_pos: Tutorial.pos,
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
    // let description_view =
    //   CellCommon.narrative_cell(
    //     div(~attrs=[Attr.class_("cell-prompt")], [text(eds.description)]),
    //   );

    // let pre_title_view = CellCommon.title_cell(" ");
    let prompt_view = {
      let prompt_placeholder = eds.prompt == "" ? "Empty Prompt" : eds.prompt;
      let (msg, _) =
        ExplainThis.mk_translation(~globals, prompt_placeholder);
      div(~attrs=[Attr.class_("prompt-content")], msg);
    };
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
    // Printf.printf(
    //   "User Impl After View: %s\n",
    //   CellEditor.Model.show(user_impl),
    // ):

    let hidden_tests_view =
      InstructorOnly(
        () => editor_view(HiddenTests, hidden_tests, ~caption="Hidden Tests"),
      );
    let hint_view =
      switch (grading_report.impl_grading_report.hints) {
      | [hint] =>
        div(
          ~attrs=[Attr.class_("hint-cell")],
          [
            div(~attrs=[Attr.class_("hint-title")], [text("💡 Hint")]),
            div(~attrs=[Attr.class_("hint-content")], [text(hint)]),
          ],
        )
      | _ => div([]) // No hint available
      };
    let impl_grading_view =
      Always(
        if (test_count == 1) {
          Printf.printf("show report: %b\n", eds.show_report);
          let checkmark_view =
            switch (Tutorial.get_stitched(HiddenTests, stitched_tests)) {
            | Some(test_results) =>
              if (test_results.total == 1 && test_results.passing == 1) {
                // ✅ Test case has passed -> Show check mark next to hint with a button
                div(
                  ~attrs=[Attr.class_("checkmark-container")],
                  [
                    div(
                      ~attrs=[Attr.class_("checkmark")],
                      [text("✔️")],
                    ),
                    div(
                      ~attrs=[Attr.class_("report-icon")],
                      [
                        Widgets.button(Icons.info, _ =>
                          inject(Change_report_view)
                        ),
                      ],
                    ),
                  ],
                );
              } else {
                div(
                  [] // Don't show check mark if test hasn't passed
                );
              }
            | None => div([]) // No test results available yet
            };

          // ✅ Fix: Pass children as last positional argument instead of using `~children`
          div([
            checkmark_view,
            eds.show_report
              ? TutorialGrading.ImplGradingReport.view(
                  ~signal_jump=
                    id =>
                      inject(
                        Editor(
                          HiddenTests,
                          MainEditor(Perform(Jump(TileId(id)))),
                        ),
                      ),
                  ~report=grading_report.impl_grading_report,
                  ~max_points=1,
                )
              : div([]),
          ]);
        } else if (test_count > 1) {
          TutorialGrading.ImplGradingReport.view(
            ~signal_jump=
              id =>
                inject(
                  Editor(
                    HiddenTests,
                    MainEditor(Perform(Jump(TileId(id)))),
                  ),
                ),
            ~report=grading_report.impl_grading_report,
            ~max_points=1,
          );
        } else {
          div(
            [] // Ensure nothing appears if test_count is 0
          );
        },
      );
    [title_view, prompt_view, hint_view]
    @ render_cells(
        globals.settings,
        []
        @ [
          your_impl_view,
          // raw_result_view,
          hidden_tests_view,
          impl_grading_view,
        ],
      );
  };
};
