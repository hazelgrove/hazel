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

let programming_view =
    (
      ~inject,
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~exercise: Exercise.Programming.state,
      ~results,
      ~highlights,
    ) => {
  let Exercise.Programming.{eds, pos} = exercise;
  let stitched_dynamics =
    Exercise.stitch_dynamic(
      settings.core,
      Programming(exercise),
      settings.core.dynamics ? Some(results) : None,
    );

  let stitched_dynamics =
    switch (stitched_dynamics) {
    | Programming(stitched_dynamics) => stitched_dynamics
    | Proof(_) => failwith("Impossible")
    };
  let Exercise.Programming.{
        test_validation,
        user_impl,
        user_tests,
        prelude,
        instructor,
        hidden_bugs,
        hidden_tests: _,
      } = stitched_dynamics;

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
      ~selected=(Programming(pos): Exercise.pos) == this_pos,
      ~error_ids=
        Statics.Map.error_ids(editor.state.meta.term_ranges, di.info_map),
      ~inject,
      ~ui_state,
      ~mousedown_updates=[SwitchEditor(this_pos)],
      ~settings,
      ~highlights,
      ~caption=Cell.caption(caption, ~rest=?subcaption),
      ~target_id=Exercise.show_pos(this_pos),
      ~test_results=ModelResult.test_results(di.result),
      ~footer?,
      editor,
    );
  };

  let title_view = Cell.title_cell(eds.header.title);

  let prompt_view =
    Cell.narrative_cell(
      div(~attr=Attr.class_("cell-prompt"), [eds.header.prompt]),
    );

  let prelude_view =
    Always(
      editor_view(
        Programming(Prelude),
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
          Programming(CorrectImpl),
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
              instructor.info_map,
            );
          let prelude_trailing_hole_ctx =
            Haz3lcore.Editor.trailing_hole_ctx(eds.prelude, prelude.info_map);
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
              CtxInspector.ctx_view(~inject, specific_ctx)
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
        Programming(YourTests(Validation)),
        ~caption="Test Validation",
        ~subcaption=": Your Tests vs. Correct Implementation",
        ~editor=eds.your_tests.tests,
        ~di=test_validation,
        ~footer=[
          Grading.TestValidationReport.view(
            ~inject,
            grading_report.test_validation_report,
            grading_report.point_distribution.test_validation,
          ),
        ],
      ),
    );

  let wrong_impl_views =
    List.mapi(
      (i, (Exercise.Programming.{impl, _}, di)) => {
        InstructorOnly(
          () =>
            editor_view(
              Programming(HiddenBugs(i)),
              ~caption="Wrong Implementation " ++ string_of_int(i + 1),
              ~editor=impl,
              ~di,
            ),
        )
      },
      List.combine(eds.hidden_bugs, hidden_bugs),
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
        Programming(YourImpl),
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
            ~result_key=Exercise.Programming.user_impl_key,
          ),
      ),
    );
  };

  let syntax_grading_view =
    Always(Grading.SyntaxReport.view(grading_report.syntax_report));

  let impl_validation_view =
    Always(
      editor_view(
        Programming(YourTests(Testing)),
        ~caption="Implementation Validation",
        ~subcaption=
          ": Your Tests (code synchronized with Test Validation cell above) vs. Your Implementation",
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
          Programming(HiddenTests),
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
      ),
    );

  // TODO(zhiyao): derivation_view not well integrated
  [score_view, title_view, prompt_view]
  @ render_cells(
      settings,
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

let proof_view =
    (
      ~inject,
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~exercise: Exercise.Proof.state,
      ~results,
      ~highlights,
    ) => {
  let Exercise.Proof.{eds, pos} = exercise;
  let stitched_dynamics =
    Exercise.stitch_dynamic(
      settings.core,
      Proof(exercise),
      settings.core.dynamics ? Some(results) : None,
    );
  let stitched_dynamics =
    switch (stitched_dynamics) {
    | Proof(stitched_dynamics) => stitched_dynamics
    | Programming(_) => failwith("Impossible")
    };

  let Exercise.Proof.{derivation_tree, prelude, _} = stitched_dynamics;

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
      ~selected=(Proof(pos): Exercise.pos) == this_pos,
      ~error_ids=
        Statics.Map.error_ids(editor.state.meta.term_ranges, di.info_map),
      ~inject,
      ~ui_state,
      ~mousedown_updates=[SwitchEditor(this_pos)],
      ~settings,
      ~highlights,
      ~caption=Cell.caption(caption, ~rest=?subcaption),
      ~target_id=Exercise.show_pos(this_pos),
      ~test_results=ModelResult.test_results(di.result),
      ~footer?,
      editor,
    );
  };

  let title_view = Cell.title_cell(eds.header.title);

  let prompt_view =
    Cell.narrative_cell(
      div(~attr=Attr.class_("cell-prompt"), [eds.header.prompt]),
    );

  let derivation_view_maker =
      (({Exercise.Proof.jdmt, rule}, (pos, di)), child) => {
    let ed = jdmt;
    let di_child = child |> List.map(snd);
    let res =
      Grading.ProofReport.DerivationReport.verify_single(di, rule, di_child);
    (
      div(
        ~attr=Attr.class_("derivation-block"),
        [
          div(
            ~attr=Attr.class_("derivation-premises"),
            (child |> List.map(fst))
            @ [
              select(
                ~attr=
                  Attr.on_change((_, name) =>
                    inject(
                      UpdateAction.SwitchDerivationRule(
                        Proof(Derive(pos)),
                        Derivation.Rule.of_string(name),
                      ),
                    )
                  ),
                List.map(
                  r =>
                    Derivation.Rule.show(r)
                    |> EditorModeView.option_view(Derivation.Rule.show(rule)),
                  [
                    Derivation.Rule.Assumption,
                    Derivation.Rule.And_I,
                    Derivation.Rule.And_E_L,
                    Derivation.Rule.And_E_R,
                    Derivation.Rule.Or_I_L,
                    Derivation.Rule.Or_I_R,
                    Derivation.Rule.Or_E,
                    Derivation.Rule.Implies_I,
                    Derivation.Rule.Implies_E,
                    Derivation.Rule.Truth_I,
                    Derivation.Rule.Falsity_E,
                  ],
                  // | Assumption
                  // | And_I
                  // | And_E_L
                  // | And_E_R
                  // | Or_I_L
                  // | Or_I_R
                  // | Or_E
                  // | Implies_I
                  // | Implies_E
                  // | Truth_I
                  // | Falsity_E;
                ),
              ),
              text(
                res
                |> (
                  fun
                  | Ok(_) => "✅"
                  | Error(External("E-252")) => "✋"
                  | Error(External(_)) => "⌛️"
                  | Error(e) => "❌" ++ DerivationError.VerErr.repr(e)
                ),
              ),
            ],
          ),
          div(
            ~attr=Attr.class_("derivation-conclusion"),
            [
              editor_view(
                Proof(Derive(pos)),
                ~caption="Derivation",
                ~editor=ed,
                ~di,
              ),
            ],
          ),
        ],
      ),
      di,
    );
  };

  // (ed * rule) * (pos * di)
  let combined_tree =
    Util.Tree.combine((
      eds.derivation_tree,
      Util.Tree.mapi((pos, t) => (pos, t), derivation_tree),
    ));

  let derivation_view =
    div(
      ~attr=Attr.class_("cell"),
      [
        div(
          ~attr=Attr.class_("cell-item"),
          [
            Cell.caption("Derivation"),
            div(
              ~attr=Attr.class_("cell-derivation"),
              [
                fst(
                  Util.Tree.fold_deep(derivation_view_maker, combined_tree),
                ),
              ],
            ),
          ],
        ),
      ],
    );

  // Note(zhiyao): construction area ends

  let prelude_view =
    Always(
      editor_view(
        Proof(Prelude),
        ~caption="Prelude",
        ~subcaption=settings.instructor_mode ? "" : " (Read-Only)",
        ~editor=eds.prelude,
        ~di=prelude,
      ),
    );

  // TODO(zhiyao): derivation_view not well integrated
  [title_view, prompt_view]
  @ render_cells(settings, [prelude_view, Always(derivation_view)]);
};

let view =
    (
      ~inject,
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~exercise: Exercise.state,
      ~results,
      ~highlights,
    ) => {
  switch (exercise) {
  | Programming(exercise) =>
    programming_view(
      ~inject,
      ~ui_state,
      ~settings,
      ~exercise,
      ~results,
      ~highlights,
    )
  | Proof(exercise) =>
    proof_view(
      ~inject,
      ~ui_state,
      ~settings,
      ~exercise,
      ~results,
      ~highlights,
    )
  };
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

let instructor_export = (exercise: Exercise.state) =>
  switch (exercise) {
  | Programming(exercise) =>
    Widgets.button_named(
      Icons.star,
      _ => {
        // .ml files because show uses OCaml syntax (dune handles seamlessly)
        let module_name = exercise.eds.header.module_name;
        let filename = exercise.eds.header.module_name ++ ".ml";
        let content_type = "text/plain";
        let contents =
          Exercise.export_module(module_name, Programming(exercise.eds));
        JsUtil.download_string_file(~filename, ~content_type, ~contents);
        Virtual_dom.Vdom.Effect.Ignore;
      },
      ~tooltip="Export Exercise Module",
    )
  | Proof(_) =>
    Widgets.button_named(
      Icons.star,
      _ => Virtual_dom.Vdom.Effect.Ignore,
      ~tooltip="Export Exercise Module (Not Implemented yet)",
    )
  };

let instructor_transitionary_export = (exercise: Exercise.state) =>
  switch (exercise) {
  | Programming(exercise) =>
    Widgets.button_named(
      Icons.star,
      _ => {
        // .ml files because show uses OCaml syntax (dune handles seamlessly)
        let module_name = exercise.eds.header.module_name;
        let filename = exercise.eds.header.module_name ++ ".ml";
        let content_type = "text/plain";
        let contents =
          Exercise.export_transitionary_module(
            module_name,
            Programming(exercise.eds),
          );
        JsUtil.download_string_file(~filename, ~content_type, ~contents);
        Virtual_dom.Vdom.Effect.Ignore;
      },
      ~tooltip="Export Transitionary Exercise Module",
    )
  | Proof(_) =>
    Widgets.button_named(
      Icons.star,
      _ => Virtual_dom.Vdom.Effect.Ignore,
      ~tooltip="Export Transitionary Exercise Module (Not Implemented yet)",
    )
  };

let instructor_grading_export = (exercise: Exercise.state) =>
  switch (exercise) {
  | Programming(exercise) =>
    Widgets.button_named(
      Icons.star,
      _ => {
        // .ml files because show uses OCaml syntax (dune handles seamlessly)
        let module_name = exercise.eds.header.module_name;
        let filename = exercise.eds.header.module_name ++ "_grading.ml";
        let content_type = "text/plain";
        let contents =
          Exercise.export_grading_module(
            module_name,
            Programming(exercise.eds),
          );
        JsUtil.download_string_file(~filename, ~content_type, ~contents);
        Virtual_dom.Vdom.Effect.Ignore;
      },
      ~tooltip="Export Grading Exercise Module",
    )
  | Proof(_) =>
    Widgets.button_named(
      Icons.star,
      _ => Virtual_dom.Vdom.Effect.Ignore,
      ~tooltip="Export Grading Exercise Module (Not Implemented yet)",
    )
  };

let download_editor_state = (~instructor_mode) =>
  Log.get_and(log => {
    let data = Export.export_all(~instructor_mode, ~log);
    JsUtil.download_json(ExerciseSettings.filename, data);
  });

let export_submission = (~settings: Settings.t) =>
  Widgets.button_named(
    Icons.star,
    _ => {
      download_editor_state(~instructor_mode=settings.instructor_mode);
      Virtual_dom.Vdom.Effect.Ignore;
    },
    ~tooltip="Export Submission",
  );

let import_submission = (~inject) =>
  Widgets.file_select_button_named(
    "import-submission",
    Icons.star,
    file => {
      switch (file) {
      | None => Virtual_dom.Vdom.Effect.Ignore
      | Some(file) => inject(UpdateAction.InitImportAll(file))
      }
    },
    ~tooltip="Import Submission",
  );
