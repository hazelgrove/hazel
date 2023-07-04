open Haz3lcore;

let go =
    (
      model: Model.t,
      action: Auto.action(Auto.llm_report),
      ~schedule_action: UpdateAction.t => unit,
    ) => {
  switch (action) {
  | StartRun () =>
    print_endline("RUN: starting");
    switch (model.meta.auto) {
    | {current_script: Some(_), _}
    | {to_run: [_, ..._], _} =>
      print_endline("RUN: Error: run already in progress");
      model.meta;
    | _ =>
      schedule_action(SetMeta(Auto(StartTest()))); //TODO: which test?
      let ctx_init = Editors.get_ctx_init(model.editors);
      let auto: UpdateAction.auto_llm = {
        current_script: None,
        to_run: Scripter.test_scripts(~ctx_init),
        reports: VarMap.empty,
      };
      {...model.meta, auto};
    };
  | StartTest () =>
    switch (model.meta.auto) {
    | {current_script: Some(_), _} =>
      print_endline("SCRIPT: Error: previous test still in progress");
      model.meta;
    | {to_run: [], reports, _} =>
      print_endline("SCRIPT: Error: no tests left to run. Results:");
      print_endline(Auto.show_reports(Auto.pp_llm_report, reports));
      model.meta;
    | {to_run: [(name, s1), ...to_run], reports, _} =>
      print_endline("SCRIPT: Starting script: " ++ name);
      List.iter(schedule_action, s1);
      let auto: UpdateAction.auto_llm = {
        current_script: Some(name),
        to_run,
        reports,
      };
      schedule_action(
        SetMeta(Auto(UpdateResult(name, Auto.init_llm_report))),
      );
      {...model.meta, auto};
    }
  | EndTest () =>
    switch (model.meta.auto) {
    | {current_script: None, _} =>
      print_endline("SCRIPT: EndTest: Error: no test in progress");
      model.meta;
    | {current_script: Some(name), to_run: _, reports: _, _} =>
      print_endline("SCRIPT: Ending script: " ++ name);
      //TODO(andrew): abstract this script into cleanup function
      schedule_action(Agent(AcceptSuggestion));
      schedule_action(SetMeta(Auto(LogTest())));
      model.meta;
    }
  | UpdateResult(name, updater) =>
    let reports =
      switch (VarMap.lookup(model.meta.auto.reports, name)) {
      | None =>
        print_endline(
          "Script: UpdateResult: Creating new report entry for: " ++ name,
        );
        VarMap.extend(
          model.meta.auto.reports,
          (name, updater(Auto.blank_llm_report)),
        );
      | Some(_) =>
        print_endline(
          "Script: UpdateResult: Updating existing report entry for: " ++ name,
        );
        VarMap.update(model.meta.auto.reports, name, updater);
      };
    {
      ...model.meta,
      auto: {
        ...model.meta.auto,
        reports,
      },
    };
  | LogTest () =>
    switch (model.meta.auto) {
    | {current_script: Some(name), to_run, reports, _} =>
      let editor = Editors.get_editor(model.editors);
      let ctx_init = Editors.get_ctx_init(model.editors);
      print_endline("SCRIPT: LogTest: Logging script: " ++ name);
      let info_map =
        ChatLSP.get_info_from_zipper(~ctx_init, editor.state.zipper);
      let syntax_errors = []; //TODO(andrew): see Filler.re (figure out how to get orphans)
      let static_errors = Statics.collect_errors(info_map);
      let completed_sketch = Printer.to_string_editor(editor);
      //TODO(andrew): use Printer.selection at an opportune time to get just the completion? or could diff it out
      schedule_action(
        SetMeta(
          Auto(
            UpdateResult(
              name,
              Auto.finalize_llm_reports(
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
        ...model.meta,
        auto: {
          current_script: None,
          to_run,
          reports,
        },
      };
    | {current_script: None, _} =>
      print_endline("SCRIPT: LogTest: Error: no test in progress");
      model.meta;
    }
  };
};
