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
      |> List.map(((id, instance_report)) =>
           switch (instance_report) {
           | [] => (id, TestStatus.Indet)
           | [(_, status, _), ..._] => (id, status)
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
      {settings, meta, _} as model: Model.t,
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
      let ctx_init = Editors.get_ctx_init(~settings, model.editors);
      let auto: UpdateAction.auto_llm = {
        current_script: None,
        to_run: Scripter.test_scripts(~settings, ~ctx_init),
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
    | {to_run: [(name, s1), ...to_run], reports, _} =>
      print_endline("AUTO: StartTest: Starting script: " ++ name);
      List.iter(schedule_action, s1);
      let auto: UpdateAction.auto_llm = {
        current_script: Some(name),
        to_run,
        reports,
      };
      schedule_action(
        SetMeta(Auto(UpdateResult(name, Auto.init_llm_report))),
      );
      {...meta, auto};
    }
  | EndTest () =>
    switch (meta.auto) {
    | {current_script: None, _} =>
      print_endline("AUTO: EndTest: Error: no test in progress");
      meta;
    | {current_script: Some(name), to_run: _, reports: _, _} =>
      print_endline("AUTO: EndTest: Ending script: " ++ name);
      //TODO(andrew): abstract this script into cleanup function
      schedule_action(Assistant(AcceptSuggestion));
      schedule_action(SetMeta(Auto(LogTest())));
      meta;
    }
  | UpdateResult(name, updater) =>
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
      let editor = Editors.get_editor(model.editors);
      let ctx_init = Editors.get_ctx_init(~settings, model.editors);
      print_endline("AUTO: LogTest: Logging script: " ++ name);
      let info_map =
        ChatLSP.get_info_from_zipper(
          ~settings,
          ~ctx_init,
          editor.state.zipper,
        );
      let syntax_errors = []; //TODO(andrew): see Filler.re (figure out how to get orphans)
      let static_errors = Statics.collect_errors(info_map);
      let completed_sketch = Printer.to_string_editor(editor);
      let test_slide = 7; //TODO(andrew): put somewhere better
      let tests = get_test_results(model, ~test_slide);
      //TODO(andrew): use Printer.selection at an opportune time to get just the completion? or could diff it out
      schedule_action(
        SetMeta(
          Auto(
            UpdateResult(
              name,
              Auto.complete_llm_reports(
                tests,
                syntax_errors,
                static_errors,
                completed_sketch,
              ),
            ),
          ),
        ),
      );
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
