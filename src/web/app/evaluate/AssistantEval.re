open Haz3lcore;
open Util;
open AssistantUpdateAction;

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
    // llm: LLM.t,
    // ...
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cases: list(case),
    curr_case: option(case),
  };

  // (Re)initializes the model with all the cases to evaluate
  let init = (): t => {
    // todo: have an intializer that takes all parameter sets and takes a cartesian product
    //       between all our prompts, initial sketches, etc etc... i.e. sets of params

    let cases =
      SketchPrompt.self
      |> List.map(scenario =>
           ToolKit.self
           |> List.map(tool_kit =>
                /* Add more mappings here if needed*/ {
                  scenario,
                  tool_kit,
                }
              )
         )
      |> List.flatten;

    {
      cases,
      curr_case: ListUtil.hd_opt(cases),
    };
  };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    // Called externally (currently from octopus button in nut-menu dropdown), (re)initializes this evaluation model
    | Init
    /* ------ Evaluation Loop - Iteratively evaluates each case --------- */
    // Prepares a fresh sketch and chat
    | PrepTest
    // Sends the prompt to the assistant
    | SendRequest
    // Called externally when the assistant is done,
    // collects metrics, pops the curr_case off cases, and begins evaluating the next case
    | CollectResults;
  /* -------------------------------------------------------------------- */

  let can_undo = (action: t) => {
    switch (action) {
    | Init
    | PrepTest
    | SendRequest
    | CollectResults => true // setting these to true is kinda helpful for debugging
    };
  };

  let update =
      (
        ~model: Model.t,
        ~action: t,
        ~assistant_model: AssistantModel.t,
        ~schedule_action: t => unit,
        ~schedule_assistant_action: AssistantUpdate.t => unit,
        ~schedule_editor_action: Editors.Update.t => unit,
      )
      : Updated.t(Model.t) => {
    let curr_case = Option.get(model.curr_case);
    let sketch = curr_case.scenario.sketch;
    let prompt = curr_case.scenario.prompt;
    switch (action) {
    | Init =>
      // todo: (re)initialize the model
      schedule_action(PrepTest);
      Model.init() |> Updated.return;

    | PrepTest =>
      print_endline("Here #0 : Init");
      // Create a new chat
      schedule_assistant_action(ChatAction(NewChat));
      // Create a new scratchpad
      schedule_editor_action(Editors.Update.Scratch(AddSlide));
      // Paste the initial sketch
      schedule_editor_action(
        Editors.Update.Scratch(
          CellAction(
            CellEditor.Update.MainEditor(
              CodeEditable.Update.Perform(Action.Paste(String(sketch))),
            ),
          ),
        ),
      );
      // Run the editor
      // This is an intermediate step so that the assistant model updates and we are on the new chat
      schedule_action(SendRequest);
      model |> Updated.return;

    | SendRequest =>
      // Send the prompt to the assistant
      schedule_assistant_action(
        AssistantUpdateAction.SendMessage(
          Composition(Request(prompt), true),
          None,
          assistant_model.current_chats.curr_composition_chat,
        ),
      );
      model |> Updated.return;

    // Await for assistant to complete the task completion
    // todo: can set tool call/token limit constraints, but this must be done in AssistantUpdate.re
    | CollectResults =>
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
        schedule_action(PrepTest);
        new_model |> Updated.return;
      } else {
        // Done!
        model |> Updated.return;
      };
    };
  };
};
