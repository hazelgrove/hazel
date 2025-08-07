open Haz3lcore;
open Util;

open AssistantEvalParams;

module Model = {
  // An evaluation suite
  // The idea is, we have a series of tests to run

  // 08/06/2025: For now, we give a proof of concept with two possible eval cases
  //             These simply just being the same sketch with two different prompts

  [@deriving (show({with_path: false}), sexp, yojson)]
  type case = {
    scenario: SketchPrompt.t,
    tool_kit: CompositionTools.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cases: list(case),
    curr_case: option(case),
  };

  let init = (): t => {
    // todo: have an intializer that reads from data and takes a cartesian prod
    //       between all our prompts, initial sketches, etc etc... i.e. sets of params
    let case1 = {
      scenario: SketchPrompt.combo_1,
      tool_kit: ToolKit.all_tools,
    };
    let case2 = {
      scenario: SketchPrompt.combo_2,
      tool_kit: ToolKit.all_tools,
    };
    let cases = [case1, case2];
    {
      cases,
      curr_case: ListUtil.hd_opt(cases),
    };
  };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Init
    | Run
    | Collect;

  let can_undo = (action: t) => {
    switch (action) {
    | Init
    | Run
    | Collect => true
    };
  };

  let update =
      (
        ~model: Model.t,
        ~action: t,
        ~assistant_model: AssistantModel.t,
        ~schedule_action: t => unit,
        ~schedule_assistant_action: AssistantUpdateUtil.t => unit,
        ~schedule_editor_action: Editors.Update.t => unit,
      )
      : Updated.t(Model.t) => {
    let curr_case = Option.get(model.curr_case);
    let sketch = curr_case.scenario.sketch;
    let prompt = curr_case.scenario.prompt;
    switch (action) {
    | Init =>
      print_endline("Here #0 : Init");
      // Create a new chat
      schedule_assistant_action(AssistantUpdateUtil.ChatAction(NewChat));
      // Create a new scratchpad
      schedule_editor_action(Editors.Update.Scratch(AddSlide));
      // Paste the initial sketch
      let a = Action.Paste(String(sketch));
      let perform_action = CodeEditable.Update.Perform(a);
      let cell_action = CellEditor.Update.MainEditor(perform_action);
      let scratch_action = Editors.Update.Scratch(CellAction(cell_action));
      schedule_editor_action(scratch_action);
      // Run the editor
      // This is an intermediate step so that the assistant model propogates and we are on new chat
      // This is mainly because we have assistant_model as an input parameter
      schedule_action(Run);
      model |> Updated.return;

    | Run =>
      // Send the prompt to the assistant
      schedule_assistant_action(
        AssistantUpdateUtil.SendMessage(
          Composition(Request(prompt)),
          None,
          assistant_model.current_chats.curr_composition_chat,
        ),
      );
      model |> Updated.return;

    // Await for assistant to complete the task completion
    // todo: can set tool call/token limit constraints, but this must be done in AssistantUpdate.re
    | Collect =>
      // -----------------------------------------------------------------------
      // todo: Decide what results to collect. We may be able to just store them
      //       in a case itself, and export them later to a json file.
      // -----------------------------------------------------------------------
      // Pop this case off the list and set the curr_case to the next one
      let new_cases = List.tl(model.cases);
      let new_model: Model.t = {
        cases: new_cases,
        curr_case: ListUtil.hd_opt(new_cases),
      };
      if (List.length(new_cases) > 0) {
        // Evaluate the next case
        schedule_action(Init);
        new_model |> Updated.return;
      } else {
        // Done!
        model |> Updated.return;
      };
    };
  };
};
