open Haz3lcore;

let get_test_results =
    (model: Model.t, ~test_slide: int): option(Auto.test_results) =>
  switch (model.editors) {
  | Scratch(_idx, slides) =>
    let settings = model.settings;
    let editors = Editors.Scratch(test_slide, slides);
    let ctx_init = Editors.get_ctx_init(~settings, editors);
    let env_init = Editors.get_env_init(~settings, editors);
    let editor = Editors.get_editor(editors);
    try(
      Interface.eval_z(
        ~settings=settings.core,
        ~env_init,
        ~ctx_init,
        editor.state.zipper,
      )
      |> ProgramResult.get_state
      |> EvaluatorState.get_tests
      |> List.map(((_id, instance_report)) =>
           switch (instance_report) {
           | [] => TestStatus.Indet
           | [(_, status, _), ..._] => status
           }
         )
      |> Option.some
    ) {
    | _ =>
      print_endline(
        "AUTO: ERROR: get_test_results: exception during evaluation",
      );
      None;
    };
  | _ => None
  };

let go =
    (
      {meta, _} as model: Model.t,
      action: Auto.action(Auto.llm_report),
      ~schedule_action: UpdateAction.t => unit,
    ) => {
  switch (action) {
  | StartRun () =>
    print_endline("AUTO: StartRun");
    switch (meta.auto) {
    | {current_script: Some(_), _}
    | {to_run: [_, ..._], _} =>
      print_endline("AUTO: StartRun: Error: run already in progress");
      meta;
    | _ =>
      schedule_action(SetMeta(Auto(StartTest()))); //TODO: which test?
      //let ctx_init = Editors.get_ctx_init(~settings, model.editors);
      let auto: UpdateAction.auto_llm = {
        current_script: None,
        to_run: Scripter.test_scripts,
        reports: VarMap.empty,
      };
      print_endline(
        "AUTO: StartRun: Number of scripts to run: "
        ++ (auto.to_run |> List.length |> string_of_int),
      );
      {...meta, auto};
    };
  | StartTest () =>
    switch (meta.auto) {
    | {current_script: Some(_), _} =>
      print_endline(
        "AUTO: StartTest: Error: previous test still in progress",
      );
      meta;
    | {to_run: [], reports, _} =>
      print_endline("AUTO: StartTest: Finished all tests. Results:");
      //print_endline(Auto.show_reports(Auto.pp_llm_report, reports));
      let statuses =
        reports |> VarMap.map(((_name, guy)) => Auto.final_report(guy));
      let json_report = Auto.yojson_of_final_statuses(statuses);
      JsUtil.download_json("hazel-llm-auto-results", json_report);
      meta;
    | {to_run: [(name, (options, actions)), ...to_run], reports, _} =>
      print_endline("AUTO: StartTest: Starting script: " ++ name);
      List.iter(schedule_action, actions);
      let auto: UpdateAction.auto_llm = {
        current_script: Some(name),
        to_run,
        reports,
      };
      schedule_action(SetMeta(Auto(UpdateResult(name, Init(options)))));
      {...meta, auto};
    }
  | EndTest () =>
    switch (meta.auto.current_script) {
    | None =>
      print_endline("AUTO: EndTest: Error: no test in progress");
      meta;
    | Some(name) =>
      print_endline("AUTO: EndTest: Ending script: " ++ name);
      //TODO(andrew): abstract this script into cleanup function
      schedule_action(Assistant(AcceptSuggestion));
      schedule_action(SetMeta(Auto(LogTest())));
      meta;
    }
  | UpdateResult(name, updater) =>
    let updater =
      switch (updater) {
      | Init(options) => Auto.init_llm_report(options)
      | Complete(tests) => Auto.complete_llm_reports(tests)
      | AddRoundOne(settings, init_ctx, mode, reply) =>
        Auto.add_first_round_results(
          Filler.mk_round_report(~settings, ~init_ctx, ~mode, reply),
        )
      | AddRoundTwo(settings, init_ctx, mode, reply) =>
        Auto.add_second_round_results(
          Filler.mk_round_report(~settings, ~init_ctx, ~mode, reply),
        )
      };
    let reports =
      switch (VarMap.lookup(meta.auto.reports, name)) {
      | None =>
        print_endline(
          "AUTO: UpdateResult: Creating new report entry for: " ++ name,
        );
        VarMap.extend(
          meta.auto.reports,
          (name, updater(Auto.blank_llm_report)),
        );
      | Some(_) =>
        print_endline(
          "AUTO: UpdateResult: Updating existing report entry for: " ++ name,
        );
        VarMap.update(meta.auto.reports, name, updater);
      };
    {
      ...meta,
      auto: {
        ...meta.auto,
        reports,
      },
    };
  | LogTest () =>
    switch (meta.auto) {
    | {current_script: Some(name), to_run, reports, _} =>
      print_endline("AUTO: LogTest: Logging script: " ++ name);
      let test_slide = 6; //TODO(andrew): put somewhere better
      let tests = get_test_results(model, ~test_slide);
      //TODO(andrew): use Printer.selection at an opportune time to get just the completion? or could diff it out
      schedule_action(SetMeta(Auto(UpdateResult(name, Complete(tests)))));
      schedule_action(SetMeta(Auto(StartTest())));
      {
        ...meta,
        auto: {
          current_script: None,
          to_run,
          reports,
        },
      };
    | {current_script: None, _} =>
      print_endline("AUTO: LogTest: Error: no test in progress");
      meta;
    }
  };
};
