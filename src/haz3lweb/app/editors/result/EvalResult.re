open Virtual_dom.Vdom;
open Node;
open Util;

module type Model = {
  type t;
};

/* The result box at the bottom of a cell. This is either the TestResutls
   kind where only a summary of test results is shown, or the EvalResults kind
   where users can choose whether they want to use a single-stepper or see the
   result of full evaluation. */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type evaluated_result = {
    // Updated
    exp: Haz3lcore.Exp.t,
    state: Haz3lcore.EvaluatorState.t,
    // Calculated
    editor: CodeSelectable.Model.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type result =
    | NoElab
    | Evaluation({
        elab: Haz3lcore.Exp.t,
        result: Haz3lcore.ProgramResult.t(evaluated_result),
      })
    | Stepper(Stepper.Model.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type kind =
    | Evaluation
    | Stepper;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    kind,
    result,
  };

  let make_test_report = (model: t): option(Haz3lcore.TestResults.t) =>
    switch (model.result) {
    | Evaluation({result: ResultOk({state, _}), _}) =>
      Some(
        state
        |> Haz3lcore.EvaluatorState.get_tests
        |> Haz3lcore.TestResults.mk_results,
      )
    | Stepper(s) =>
      Some(
        s.history
        |> Stepper.Model.get_state
        |> Haz3lcore.EvaluatorState.get_tests
        |> Haz3lcore.TestResults.mk_results,
      )
    | Evaluation(_)
    | NoElab => None
    };

  let init = {kind: Evaluation, result: NoElab};

  let test_results = (model: t): option(Haz3lcore.TestResults.t) =>
    switch (model.result) {
    | Evaluation({result: ResultOk({state, _}), _}) =>
      Some(
        state
        |> Haz3lcore.EvaluatorState.get_tests
        |> Haz3lcore.TestResults.mk_results,
      )
    | Stepper(s) =>
      Some(
        s.history
        |> Stepper.Model.get_state
        |> Haz3lcore.EvaluatorState.get_tests
        |> Haz3lcore.TestResults.mk_results,
      )
    | Evaluation(_)
    | NoElab => None
    };

  let get_elaboration = (model: t): option(Haz3lcore.Exp.t) =>
    switch (model.result) {
    | Evaluation({elab, _}) => Some(elab)
    | Stepper(s) => Stepper.Model.get_elaboration(s)
    | _ => None
    };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | ToggleStepper
    | StepperAction(Stepper.Update.t)
    | EvalEditorAction(CodeSelectable.Update.t)
    | UpdateResult(Haz3lcore.ProgramResult.t(Haz3lcore.ProgramResult.inner));

  let update = (~settings, action, model: Model.t): Updated.t(Model.t) =>
    switch (action, model) {
    | (ToggleStepper, {kind: Stepper, _}) =>
      {...model, kind: Evaluation} |> Updated.return
    | (ToggleStepper, {kind: Evaluation, _}) =>
      {...model, kind: Stepper} |> Updated.return
    | (StepperAction(a), {result: Stepper(s), _}) =>
      let* stepper = Stepper.Update.update(~settings, a, s);
      {...model, result: Stepper(stepper)};
    | (StepperAction(_), _) => model |> Updated.return_quiet
    | (
        EvalEditorAction(a),
        {
          result: Evaluation({elab, result: ResultOk({editor, state, exp})}),
          _,
        },
      ) =>
      let* ed' = CodeSelectable.Update.update(~settings, a, editor);
      {
        ...model,
        result:
          Evaluation({elab, result: ResultOk({editor: ed', state, exp})}),
      };
    | (EvalEditorAction(_), _) => model |> Updated.return_quiet
    | (UpdateResult(update), {result: Evaluation({elab, _}), _}) =>
      {
        ...model,
        result:
          Evaluation({
            elab,
            result:
              Haz3lcore.ProgramResult.map(
                ({result: r, state: s}: Haz3lcore.ProgramResult.inner) => {
                  let exp = Haz3lcore.ProgramResult.Result.unbox(r);
                  let editor =
                    CodeSelectable.Model.mk_from_exp(
                      ~settings=settings.core,
                      exp,
                    );
                  Model.{exp, state: s, editor};
                },
                update,
              ),
          }),
      }
      |> Updated.return
    | (UpdateResult(_), _) => model |> Updated.return_quiet
    };

  let calculate =
      (
        ~settings: Haz3lcore.CoreSettings.t,
        ~queue_worker: option(Haz3lcore.Exp.t => unit),
        ~is_edited: bool,
        statics: Haz3lcore.CachedStatics.t,
        model: Model.t,
      ) => {
    let elab = statics.elaborated;
    let model =
      switch (model.kind, model.result) {
      // If elab hasn't changed, don't recalculate
      | (Evaluation, Evaluation({elab: elab', result}))
          when Haz3lcore.Exp.fast_equal(elab, elab') => {
          ...model,
          result: Evaluation({elab, result}),
        }
      // If elab has changed, recalculate
      | (Evaluation, _) when settings.dynamics =>
        switch (queue_worker) {
        | None => {
            ...model,
            result:
              Evaluation({
                elab,
                result: {
                  switch (WorkerServer.work(elab)) {
                  | Ok((r, state)) =>
                    let exp = Haz3lcore.ProgramResult.Result.unbox(r);
                    let editor =
                      CodeSelectable.Model.mk_from_exp(~settings, exp);
                    Haz3lcore.ProgramResult.ResultOk({exp, editor, state});
                  | Error(e) => Haz3lcore.ProgramResult.ResultFail(e)
                  };
                },
              }),
          }

        | Some(queue_worker) =>
          queue_worker(elab);
          {
            ...model,
            result:
              Evaluation({
                elab,
                result: Haz3lcore.ProgramResult.ResultPending,
              }),
          };
        }
      | (Evaluation, _) => {...model, result: NoElab}
      | (Stepper, Stepper(s)) =>
        let s' = Stepper.Update.calculate(~settings, elab, s);
        {...model, result: Stepper(s')};
      | (Stepper, _) =>
        let s =
          Stepper.Model.init() |> Stepper.Update.calculate(~settings, elab);
        {...model, result: Stepper(s)};
      };
    // Calculate evaluation editor
    switch (model.result) {
    | Evaluation({elab, result: ResultOk({state, exp, _})}) =>
      let editor = CodeSelectable.Model.mk_from_exp(~settings, exp);
      let editor =
        CodeSelectable.Update.calculate(
          ~settings,
          ~stitch=x => x,
          ~is_edited,
          editor,
        );
      {
        ...model,
        result: Evaluation({elab, result: ResultOk({editor, state, exp})}),
      };
    | _ => model
    };
  };
};

module Selection = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Evaluation(CodeSelectable.Selection.t)
    | Stepper(Stepper.Selection.t);
  // TODO: Selection in stepper

  let get_cursor_info = (~selection: t, mr: Model.t): cursor(Update.t) =>
    switch (selection, mr.result) {
    | (_, NoElab) => empty
    | (
        Evaluation(selection),
        Evaluation({result: ResultOk({editor, _}), _}),
      ) =>
      let+ ci = CodeSelectable.Selection.get_cursor_info(~selection, editor);
      Update.EvalEditorAction(ci);
    | (_, Evaluation(_)) => empty
    | (_, Stepper(_)) => empty
    };

  let handle_key_event =
      (~selection: t, ~event, mr: Model.t): option(Update.t) =>
    switch (selection, mr.result) {
    | (_, NoElab) => None
    | (
        Evaluation(selection),
        Evaluation({result: ResultOk({editor, _}), _}),
      ) =>
      CodeSelectable.Selection.handle_key_event(~selection, editor, event)
      |> Option.map(x => Update.EvalEditorAction(x))
    | (_, Evaluation(_)) => None
    | (_, Stepper(_)) => None
    };
};

module View = {
  type event =
    | MakeActive(Selection.t)
    | JumpTo(Haz3lcore.Id.t);

  let error_msg = (err: Haz3lcore.ProgramResult.error) =>
    switch (err) {
    | EvaulatorError(err) => Haz3lcore.EvaluatorError.show(err)
    | UnknownException(str) => str
    | Timeout => "Evaluation timed out"
    };

  let status_of: Haz3lcore.ProgramResult.t('a) => string =
    fun
    | ResultPending => "pending"
    | ResultOk(_) => "ok"
    | ResultFail(_) => "fail"
    | Off(_) => "off";

  let live_eval =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected,
        ~locked,
        elab: Haz3lcore.Exp.t,
        result: Haz3lcore.ProgramResult.t(Model.evaluated_result),
      ) => {
    let editor =
      switch (result) {
      | ResultOk({editor, _}) => editor
      | _ =>
        elab
        |> CodeSelectable.Model.mk_from_exp(~settings=globals.settings.core)
      };
    let code_view =
      CodeSelectable.View.view(
        ~signal=
          fun
          | MakeActive => signal(MakeActive(Evaluation())),
        ~inject=a => inject(EvalEditorAction(a)),
        ~globals,
        ~selected,
        ~sort=Haz3lcore.Sort.root,
        editor,
      );
    let exn_view =
      switch (result) {
      | ResultFail(err) => [
          div(
            ~attrs=[Attr.classes(["error-msg"])],
            [text(error_msg(err))],
          ),
        ]
      | _ => []
      };
    Node.(
      div(
        ~attrs=[Attr.classes(["cell-item", "cell-result"])],
        exn_view
        @ [
          div(
            ~attrs=[Attr.classes(["status", status_of(result)])],
            [
              div(~attrs=[Attr.classes(["spinner"])], []),
              div(~attrs=[Attr.classes(["eq"])], [text("≡")]),
            ],
          ),
          div(
            ~attrs=[Attr.classes(["result", status_of(result)])],
            [code_view],
          ),
        ]
        @ (
          locked
            ? []
            : [
              Widgets.toggle(~tooltip="Show Stepper", "s", false, _ =>
                inject(ToggleStepper)
              ),
            ]
        ),
      )
    );
  };

  let footer =
      (
        ~globals: Globals.t,
        ~signal,
        ~inject,
        ~result: Model.t,
        ~selected: option(Selection.t),
        ~locked,
      ) =>
    switch (result.result) {
    | _ when !globals.settings.core.dynamics => []
    | NoElab => []
    | Evaluation({elab, result}) => [
        live_eval(
          ~globals,
          ~signal,
          ~inject,
          ~selected=selected == Some(Evaluation()),
          ~locked,
          elab,
          result,
        ),
      ]
    | Stepper(s) =>
      Stepper.View.view(
        ~globals,
        ~selection=
          switch (selected) {
          | Some(Stepper(s)) => Some(s)
          | _ => None
          },
        ~signal=
          fun
          | HideStepper => inject(ToggleStepper)
          | JumpTo(id) => signal(JumpTo(id))
          | MakeActive(s) => signal(MakeActive(Stepper(s))),
        ~inject=x => inject(StepperAction(x)),
        ~read_only=locked,
        s,
      )
    };

  let test_status_icon_view =
      (~font_metrics, insts, ms: Haz3lcore.Measured.Shards.t): option(Node.t) =>
    switch (ms) {
    | [(_, {origin: _, last}), ..._] =>
      let status =
        insts
        |> Haz3lcore.TestMap.joint_status
        |> Haz3lcore.TestStatus.to_string;
      let pos = DecUtil.abs_position(~font_metrics, last);
      Some(
        Node.div(~attrs=[Attr.classes(["test-result", status]), pos], []),
      );
    | _ => None
    };

  let test_result_layer =
      (
        ~font_metrics,
        ~measured: Haz3lcore.Measured.t,
        test_results: Haz3lcore.TestResults.t,
      )
      : Web.Node.t =>
    Web.div_c(
      "test-decos",
      List.filter_map(
        ((id, insts)) =>
          switch (Haz3lcore.Id.Map.find_opt(id, measured.tiles)) {
          | Some(ms) => test_status_icon_view(~font_metrics, insts, ms)
          | None => None
          },
        test_results.test_map,
      ),
    );

  type result_kind =
    | NoResults
    | TestResults
    | EvalResults
    | Custom(Node.t);

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Selection.t),
        ~result_kind=EvalResults,
        ~locked: bool,
        model: Model.t,
      ) =>
    switch (result_kind) {
    // Normal case:
    | EvalResults when globals.settings.core.dynamics =>
      let result =
        footer(~globals, ~signal, ~inject, ~result=model, ~selected, ~locked);
      let test_overlay = (editor: Haz3lcore.Editor.t) =>
        switch (Model.test_results(model)) {
        | Some(result) => [
            test_result_layer(
              ~font_metrics=globals.font_metrics,
              ~measured=editor.syntax.measured,
              result,
            ),
          ]
        | None => []
        };
      (result, test_overlay);

    // Just showing elaboration because evaluation is off:
    | EvalResults when globals.settings.core.elaborate =>
      let result = [
        text("Evaluation disabled, showing elaboration:"),
        switch (Model.get_elaboration(model)) {
        | Some(elab) =>
          elab
          |> Haz3lcore.ExpToSegment.(
               exp_to_segment(
                 ~settings=
                   Settings.of_core(~inline=false, globals.settings.core),
               )
             )
          |> CodeViewable.view_segment(
               ~globals,
               ~sort=Exp,
               ~info_map=Haz3lcore.Id.Map.empty,
             )
        | None => text("No elaboration found")
        },
      ];
      (result, (_ => []));

    // Not showing any results:
    | EvalResults
    | NoResults => ([], (_ => []))

    | Custom(node) => (
        [node],
        (
          (editor: Haz3lcore.Editor.t) =>
            switch (Model.test_results(model)) {
            | Some(result) => [
                test_result_layer(
                  ~font_metrics=globals.font_metrics,
                  ~measured=editor.syntax.measured,
                  result,
                ),
              ]
            | None => []
            }
        ),
      )

    // Just showing test results (school mode)
    | TestResults =>
      let test_results = Model.test_results(model);
      let test_overlay = (editor: Haz3lcore.Editor.t) =>
        switch (Model.test_results(model)) {
        | Some(result) => [
            test_result_layer(
              ~font_metrics=globals.font_metrics,
              ~measured=editor.syntax.measured,
              result,
            ),
          ]
        | None => []
        };
      (
        [
          CellCommon.report_footer_view([
            TestView.test_summary(
              ~inject_jump=tile => signal(JumpTo(tile)),
              ~test_results,
            ),
          ]),
        ],
        test_overlay,
      );
    };
};

let view = View.view;
