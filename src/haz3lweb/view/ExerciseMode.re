open Util;
open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

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
      ~inject,
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~exercise,
      ~stitched_dynamics,
      ~highlights,
    ) => {
  let Exercise.{eds, pos} = exercise;
  let {
    test_validation,
    user_impl,
    user_tests,
    prelude,
    instructor,
    hidden_bugs,
    hidden_tests: _,
  }:
    Exercise.stitched(Exercise.DynamicsItem.t) = stitched_dynamics;
  let grading_report = Grading.GradingReport.mk(eds, ~stitched_dynamics);
  let score_view = Grading.GradingReport.view_overall_score(grading_report);
  let editor_view =
      (
        ~editor: Editor.t,
        ~caption: string,
        ~subcaption: option(string)=?,
        ~footer=?,
        ~di: Exercise.DynamicsItem.t,
        this_pos,
      ) => {
    Cell.editor_view(
      ~selected=pos == this_pos,
      ~override_statics=di.statics,
      ~inject,
      ~ui_state,
      ~mousedown_updates=[SwitchEditor(this_pos)],
      ~settings,
      ~highlights,
      ~caption=
        switch (this_pos) {
        | HiddenBugs(n) => Cell.wrong_impl_caption(~inject, caption, n)
        | _ => Cell.caption(caption, ~rest=?subcaption)
        },
      ~target_id=Exercise.show_pos(this_pos),
      ~test_results=ModelResult.test_results(di.result),
      ~footer?,
      editor,
    );
  };

  let update_title = _ => {
    let new_title =
      Obj.magic(
        Js_of_ocaml.Js.some(JsUtil.get_elem_by_id("title-input-box")),
      )##.value;
    let update_events = [
      inject(Set(EditingTitle)),
      inject(UpdateTitle(new_title)),
    ];
    Virtual_dom.Vdom.Effect.Many(update_events);
  };

  let title_view = {
    let title_placeholder = eds.title == "" ? "Exercise Title" : eds.title;
    Cell.simple_cell_view([
      div(
        ~attrs=[Attr.class_("title-cell")],
        [
          settings.instructor_mode
            ? settings.editing_title
                ? div(
                    ~attrs=[Attr.class_("title-edit")],
                    [
                      input(
                        ~attrs=[
                          Attr.class_("title-text"),
                          Attr.id("title-input-box"),
                          Attr.value(eds.title),
                        ],
                        (),
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [Widgets.button(Icons.confirm, update_title)],
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [
                          Widgets.button(Icons.cancel, _ =>
                            inject(Set(EditingTitle))
                          ),
                        ],
                      ),
                    ],
                  )
                : div(
                    ~attrs=[Attr.class_("title-edit")],
                    [
                      div(
                        ~attrs=[
                          Attr.classes([
                            "title-text",
                            eds.title == "" ? "title-placeholder" : "",
                          ]),
                        ],
                        [text(title_placeholder)],
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [
                          Widgets.button(Icons.pencil, _ =>
                            inject(Set(EditingTitle))
                          ),
                        ],
                      ),
                    ],
                  )
            : div(~attrs=[Attr.class_("title-text")], [text(eds.title)]),
        ],
      ),
    ]);
  };

  let update_module_name = _ => {
    let new_module_name =
      Obj.magic(
        Js_of_ocaml.Js.some(JsUtil.get_elem_by_id("module-name-input")),
      )##.value;
    let update_events = [
      inject(Set(EditingModuleName)),
      inject(UpdateModuleName(new_module_name)),
    ];
    Virtual_dom.Vdom.Effect.Many(update_events);
  };

  let module_name_view = {
    let module_placeholder =
      eds.module_name == "" ? "Exercise Module Name" : eds.module_name;
    settings.instructor_mode
      ? Cell.narrative_cell([
          div(
            ~attrs=[Attr.class_("cell-module-name")],
            [
              settings.editing_module_name
                ? div(
                    ~attrs=[Attr.class_("module-name-edit")],
                    [
                      label([text("Module name:")]),
                      input(
                        ~attrs=[
                          Attr.type_("text"),
                          Attr.class_("text-input"),
                          Attr.id("module-name-input"),
                          Attr.value(eds.module_name),
                        ],
                        (),
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [Widgets.button(Icons.confirm, update_module_name)],
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [
                          Widgets.button(Icons.cancel, _ =>
                            inject(Set(EditingModuleName))
                          ),
                        ],
                      ),
                    ],
                  )
                : div(
                    ~attrs=[Attr.class_("module-name-text")],
                    [
                      text("Module name: "),
                      div(
                        ~attrs=[
                          Attr.classes([
                            eds.module_name == "" ? "module-placeholder" : "",
                          ]),
                        ],
                        [text(module_placeholder)],
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [
                          Widgets.button(Icons.pencil, _ =>
                            inject(Set(EditingModuleName))
                          ),
                        ],
                      ),
                    ],
                  ),
            ],
          ),
        ])
      : Node.none;
  };

  let update_prompt = _ => {
    let new_prompt =
      Obj.magic(
        Js_of_ocaml.Js.some(JsUtil.get_elem_by_id("prompt-input-box")),
      )##.value;
    let update_events = [
      inject(Set(EditingPrompt)),
      inject(UpdatePrompt(new_prompt)),
    ];
    Virtual_dom.Vdom.Effect.Many(update_events);
  };

  let prompt_view = {
    let prompt_placeholder = eds.prompt == "" ? "Exercise Prompt" : eds.prompt;
    let (msg, _) =
      ExplainThis.mk_translation(~inject=Some(inject), prompt_placeholder);
    Cell.narrative_cell([
      div(
        ~attrs=[Attr.class_("cell-prompt")],
        [
          settings.instructor_mode
            ? settings.editing_prompt
                ? div(
                    ~attrs=[Attr.class_("prompt-edit")],
                    [
                      textarea(
                        ~attrs=[
                          Attr.class_("prompt-text"),
                          Attr.id("prompt-input-box"),
                        ],
                        [text(eds.prompt)],
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [Widgets.button(Icons.confirm, update_prompt)],
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [
                          Widgets.button(Icons.cancel, _ =>
                            inject(Set(EditingPrompt))
                          ),
                        ],
                      ),
                    ],
                  )
                : div(
                    ~attrs=[Attr.class_("prompt-edit")],
                    [
                      div(
                        ~attrs=[
                          Attr.classes([
                            "prompt-content",
                            eds.prompt == "" ? "prompt-placeholder" : "",
                          ]),
                        ],
                        msg,
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [
                          Widgets.button(Icons.pencil, _ =>
                            inject(Set(EditingPrompt))
                          ),
                        ],
                      ),
                    ],
                  )
            : div(~attrs=[Attr.class_("prompt-content")], msg),
        ],
      ),
    ]);
  };

  let prelude_view =
    Always(
      editor_view(
        Prelude,
        ~caption="Prelude",
        ~subcaption=settings.instructor_mode ? "" : " (Read-Only)",
        ~editor=eds.prelude,
        ~di=prelude,
      ),
    );

  let correct_impl_view =
    InstructorOnly(
      () =>
        editor_view(
          CorrectImpl,
          ~caption="Correct Implementation",
          ~editor=eds.correct_impl,
          ~di=instructor,
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
              instructor.statics.info_map,
            );
          let prelude_trailing_hole_ctx =
            Haz3lcore.Editor.trailing_hole_ctx(
              eds.prelude,
              prelude.statics.info_map,
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
              ContextInspector.ctx_view(~inject, specific_ctx)
            };
          };
        };
        Cell.simple_cell_view([
          Cell.simple_cell_item([
            Cell.caption(
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
        ~caption="Test Validation",
        ~subcaption=": Your Tests vs. Correct Implementation",
        ~editor=eds.your_tests.tests,
        ~di=test_validation,
        ~footer=[
          Grading.TestValidationReport.view(
            ~inject,
            grading_report.test_validation_report,
            grading_report.point_distribution.test_validation,
            eds.your_tests.required,
            settings,
          ),
        ],
      ),
    );
  let wrong_impl_views =
    List.mapi(
      (i, (Exercise.{impl, _}, di)) => {
        editor_view(
          HiddenBugs(i),
          ~caption="Mutant " ++ string_of_int(i + 1),
          ~editor=impl,
          ~di,
        )
      },
      List.combine(eds.hidden_bugs, hidden_bugs),
    );

  let add_wrong_impl_view =
    Cell.simple_cell_view([
      Cell.simple_cell_item([
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
                  _ => inject(UpdateAction.AddBuggyImplementation),
                  ~tooltip="Add Buggy Implementation",
                ),
              ],
            ),
          ],
        ),
      ]),
    ]);

  let mutation_testing_view =
    Always(
      Grading.MutationTestingReport.view(
        ~inject,
        grading_report.mutation_testing_report,
        grading_report.point_distribution.mutation_testing,
        settings,
      ),
    );
  let your_impl_view = {
    Always(
      editor_view(
        YourImpl,
        ~caption="Your Implementation",
        ~editor=eds.your_impl,
        ~di=user_impl,
        ~footer=
          Cell.footer(
            ~locked=false,
            ~settings,
            ~inject,
            ~ui_state,
            ~result=user_impl.result,
            ~result_key=Exercise.user_impl_key,
          ),
      ),
    );
  };
  let syntax_grading_view =
    Always(Grading.SyntaxReport.view(grading_report.syntax_report));

  let impl_validation_view =
    Always(
      editor_view(
        YourTestsTesting,
        ~caption="Implementation Validation",
        ~subcaption=
          ": Your Tests (synchronized with Test Validation above) vs. Your Implementation",
        ~editor=eds.your_tests.tests,
        ~di=user_tests,
        ~footer=[
          Cell.test_report_footer_view(
            ~inject,
            ~test_results=ModelResult.test_results(user_tests.result),
          ),
        ],
      ),
    );

  let hidden_tests_view =
    InstructorOnly(
      () =>
        editor_view(
          HiddenTests,
          ~caption="Hidden Tests",
          ~editor=eds.hidden_tests.tests,
          ~di=instructor,
        ),
    );

  let impl_grading_view =
    Always(
      Grading.ImplGradingReport.view(
        ~inject,
        ~report=grading_report.impl_grading_report,
        ~syntax_report=grading_report.syntax_report,
        ~max_points=grading_report.point_distribution.impl_grading,
        ~settings,
      ),
    );

  let wrong_impl_views =
    InstructorOnly(
      () =>
        Cell.simple_cell_view([
          Cell.simple_cell_item(
            [Cell.caption("Mutation Tests")]
            @ wrong_impl_views
            @ [add_wrong_impl_view],
          ),
        ]),
    );

  [score_view, title_view, module_name_view, prompt_view]
  @ render_cells(
      settings,
      [
        prelude_view,
        correct_impl_view,
        correct_impl_ctx_view,
        your_tests_view,
        wrong_impl_views,
        mutation_testing_view,
        your_impl_view,
        syntax_grading_view,
        impl_validation_view,
        hidden_tests_view,
        impl_grading_view,
      ],
    );
};

let reset_button = inject =>
  Widgets.button_named(
    Icons.trash,
    _ => {
      let confirmed =
        JsUtil.confirm(
          "Are you SURE you want to reset this exercise? You will lose any existing code that you have written, and course staff have no way to restore it!",
        );
      if (confirmed) {
        inject(UpdateAction.ResetCurrentEditor);
      } else {
        Virtual_dom.Vdom.Effect.Ignore;
      };
    },
    ~tooltip="Reset Exercise",
  );

let instructor_export = (inject: UpdateAction.t => Ui_effect.t(unit)) =>
  Widgets.button_named(
    Icons.export,
    _ => inject(Export(ExerciseModule)),
    ~tooltip="Export Exercise Module",
  );

let instructor_transitionary_export =
    (inject: UpdateAction.t => Ui_effect.t(unit)) =>
  Widgets.button_named(
    Icons.export,
    _ => {inject(Export(TransitionaryExerciseModule))},
    ~tooltip="Export Transitionary Exercise Module",
  );

let instructor_grading_export = (inject: UpdateAction.t => Ui_effect.t(unit)) =>
  Widgets.button_named(
    Icons.export,
    _ => {inject(Export(GradingExerciseModule))},
    ~tooltip="Export Grading Exercise Module",
  );

let export_submission = (inject: UpdateAction.t => Ui_effect.t(unit)) =>
  Widgets.button_named(
    Icons.star,
    _ => inject(Export(Submission)),
    ~tooltip="Export Submission",
  );

let import_submission = (~inject) =>
  Widgets.file_select_button_named(
    "import-submission",
    Icons.import,
    file => {
      switch (file) {
      | None => Virtual_dom.Vdom.Effect.Ignore
      | Some(file) => inject(UpdateAction.InitImportAll(file))
      }
    },
    ~tooltip="Import Submission",
  );
