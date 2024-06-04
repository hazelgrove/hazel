open Virtual_dom.Vdom;
open Node;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type result =
    | NoElab
    | Evaluation({
        elab: Haz3lcore.Exp.t,
        result:
          Haz3lcore.ProgramResult.t(
            (CodeEditor.Model.t, Haz3lcore.EvaluatorState.t),
          ),
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
    | Evaluation({result: ResultOk((_, state)), _}) =>
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
    | Evaluation({result: ResultOk((_, state)), _}) =>
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
    | EvalEditorAction(CodeEditor.Update.t)
    | UpdateResult(Haz3lcore.ProgramResult.t(Haz3lcore.ProgramResult.inner));

  let update = (~settings, action, model: Model.t): Updated.t(Model.t) =>
    switch (action, model) {
    | (ToggleStepper, {kind: Stepper, _}) =>
      {...model, kind: Evaluation} |> Updated.return
    | (ToggleStepper, {kind: Evaluation, _}) =>
      {...model, kind: Stepper} |> Updated.return
    | (StepperAction(a), {result: Stepper(s), _}) =>
      {...model, result: Stepper(Stepper.Update.update(a, s))}
      |> Updated.return
    | (StepperAction(_), _) => model |> Updated.return_quiet
    | (
        EvalEditorAction(a),
        {result: Evaluation({elab, result: ResultOk((ed, st))}), _},
      ) =>
      let* ed' = CodeEditor.Update.update(~settings, a, ed);
      {...model, result: Evaluation({elab, result: ResultOk((ed', st))})};
    | (EvalEditorAction(_), _) => model |> Updated.return_quiet
    | (UpdateResult(update), {result: Evaluation({elab, _}), _}) =>
      {
        ...model,
        result:
          Evaluation({
            elab,
            result:
              Haz3lcore.ProgramResult.map(
                ({result: r, state: s}: Haz3lcore.ProgramResult.inner) =>
                  r
                  |> Haz3lcore.ProgramResult.Result.unbox
                  |> CodeEditor.Model.mk_from_exp(~settings=settings.core)
                  |> (x => (x, s)),
                update,
              ),
          }),
      }
      |> Updated.return
    | (UpdateResult(_), _) => model |> Updated.return_quiet
    };

  let calculate =
      (~settings, ~schedule_action, ~immediate, elab, model: Model.t) => {
    switch (model.kind, model.result) {
    // If elab hasn't changed, don't recalculate
    | (Evaluation, Evaluation({elab: elab', result}))
        when Haz3lcore.Exp.fast_equal(elab, elab') => {
        ...model,
        result:
          Evaluation({
            elab,
            result:
              Haz3lcore.ProgramResult.map(
                ((res, state)) =>
                  (
                    CodeEditor.Update.calculate(
                      ~settings,
                      ~stitch=x => x,
                      res,
                    ),
                    state,
                  ),
                result,
              ),
          }),
      }
    // If elab has changed, recalculate
    | (Evaluation, _) when settings.dynamics =>
      if (immediate) {
        {
          ...model,
          result:
            Evaluation({
              elab,
              result: {
                switch (WorkerServer.work(elab)) {
                | Ok((r, s)) =>
                  Haz3lcore.ProgramResult.ResultOk(
                    r
                    |> Haz3lcore.ProgramResult.Result.unbox
                    |> CodeEditor.Model.mk_from_exp(~settings)
                    |> (x => (x, s)),
                  )
                | Error(e) => Haz3lcore.ProgramResult.ResultFail(e)
                };
              },
            }),
        };
      } else {
        WorkerClient.request(
          [("", elab)],
          ~handler=
            r =>
              schedule_action(
                UpdateResult(
                  switch (r |> List.hd |> snd) {
                  | Ok((r, s)) =>
                    Haz3lcore.ProgramResult.ResultOk({result: r, state: s})
                  | Error(e) => Haz3lcore.ProgramResult.ResultFail(e)
                  },
                ),
              ),
          ~timeout=_ => schedule_action(UpdateResult(ResultFail(Timeout))),
        );
        {
          ...model,
          result:
            Evaluation({elab, result: Haz3lcore.ProgramResult.ResultPending}),
        };
      }
    | (Evaluation, _) => {...model, result: NoElab}
    | (Stepper, Stepper(s)) =>
      let s' = Stepper.Update.calculate(~settings, elab, s);
      {...model, result: Stepper(s')};
    | (Stepper, _) =>
      let s =
        Stepper.Model.init(~settings)
        |> Stepper.Update.calculate(~settings, elab);
      {...model, result: Stepper(s)};
    };
  };
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Evaluation
    | Stepper(Stepper.Selection.t);

  let get_cursor_info =
      (~selection: t, mr: Model.t): option(Haz3lcore.Info.t) =>
    switch (selection, mr.result) {
    | (_, NoElab) => None
    | (Evaluation, Evaluation({result: ResultOk((editor, _)), _})) =>
      CodeEditor.Selection.get_cursor_info(editor)
    | (_, Evaluation(_)) => None
    | (Stepper(selection), Stepper(s)) =>
      Stepper.Selection.get_cursor_info(~selection, s)
    | (_, Stepper(_)) => None
    };

  let handle_key_event =
      (~selection: t, ~event, mr: Model.t): option(Update.t) =>
    switch (selection, mr.result) {
    | (_, NoElab) => None
    | (Evaluation, Evaluation({result: ResultOk((editor, _)), _})) =>
      CodeEditor.Selection.handle_key_event(editor, event)
      |> Option.map(x => Update.EvalEditorAction(x))
    | (_, Evaluation(_)) => None
    | (Stepper(selection), Stepper(s)) =>
      Stepper.Selection.handle_key_event(~selection, ~event, s)
      |> Option.map(x => Update.StepperAction(x))
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
        result: Haz3lcore.ProgramResult.t((CodeEditor.Model.t, 'a)),
      ) => {
    let editor =
      switch (result) {
      | ResultOk((res, _)) => res
      | _ =>
        elab
        |> ReadOnlyEditor.Model.mk_from_exp(
             ~settings=globals.settings.core,
             ~inline=false,
           )
      };
    let code_view =
      CodeEditor.view(
        ~signal=
          fun
          | MakeActive => signal(MakeActive(Evaluation)),
        ~inject=a => inject(EvalEditorAction(a)),
        ~globals,
        ~selected,
        ~sort=Haz3lcore.Sort.root,
        editor,
      );
    let exn_view =
      switch (result) {
      | ResultFail(err) => [
          div(~attr=Attr.classes(["error-msg"]), [text(error_msg(err))]),
        ]
      | _ => []
      };
    Node.(
      div(
        ~attr=Attr.classes(["cell-item", "cell-result"]),
        exn_view
        @ [
          div(
            ~attr=Attr.classes(["status", status_of(result)]),
            [
              div(~attr=Attr.classes(["spinner"]), []),
              div(~attr=Attr.classes(["eq"]), [text("≡")]),
            ],
          ),
          div(
            ~attr=Attr.classes(["result", status_of(result)]),
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
          ~selected=selected == Some(Evaluation),
          ~locked,
          elab,
          result,
        ),
      ]
    | Stepper(s) =>
      Stepper.View.view(
        ~globals,
        ~signal=
          fun
          | HideStepper => inject(ToggleStepper)
          | MakeActive(x) => signal(MakeActive(Stepper(x)))
          | JumpTo(id) => signal(JumpTo(id)),
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
        Node.div(
          ~attr=Attr.many([Attr.classes(["test-result", status]), pos]),
          [],
        ),
      );
    | _ => None
    };

  let test_result_layer =
      (
        ~font_metrics,
        ~measured: Haz3lcore.Measured.t,
        test_results: Haz3lcore.TestResults.t,
      )
      : list(Node.t) =>
    List.filter_map(
      ((id, insts)) =>
        switch (Haz3lcore.Id.Map.find_opt(id, measured.tiles)) {
        | Some(ms) => test_status_icon_view(~font_metrics, insts, ms)
        | None => None
        },
      test_results.test_map,
    );

  type result_kind =
    | NoResults
    | TestResults
    | EvalResults;

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
        | Some(result) =>
          test_result_layer(
            ~font_metrics=globals.font_metrics,
            ~measured=editor.state.meta.measured,
            result,
          )
        | None => []
        };
      (result, test_overlay);

    // Just showing elaboration because evaluation is off:
    | EvalResults when globals.settings.core.elaborate =>
      let result = [
        text("Evaluation disabled, showing elaboration:"),
        switch (Model.get_elaboration(model)) {
        | Some(elab) =>
          ReadOnlyEditor.View.view(
            ~globals,
            {
              editor:
                elab |> Haz3lcore.ExpToSegment.exp_to_editor(~inline=false),
              statics: Haz3lcore.CachedStatics.empty_statics,
            }: CodeEditor.Model.t,
          )
        | None => text("No elaboration found")
        },
      ];
      (result, (_ => []));

    // Not showing any results:
    | EvalResults
    | NoResults => ([], (_ => []))

    // Just showing test results (school mode)
    | TestResults =>
      let test_results = Model.test_results(model);
      let test_overlay = (editor: Haz3lcore.Editor.t) =>
        switch (Model.test_results(model)) {
        | Some(result) =>
          test_result_layer(
            ~font_metrics=globals.font_metrics,
            ~measured=editor.state.meta.measured,
            result,
          )
        | None => []
        };
      (
        [
          Cell.report_footer_view([
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
