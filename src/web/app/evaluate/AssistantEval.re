open Haz3lcore;
open Util;

module Model = {
  // An evaluation suite
  // The idea is, we have a series of tests to run

  // 08/06/2025: For now, we give a proof of concept with two possible eval cases
  //             These simply just being the same sketch with two different prompts

  [@deriving (show({with_path: false}), sexp, yojson)]
  type case = {
    prompt: string,
    initial_sketch: string,
    final_sketch: option(string),
    // todo: add more params: tool calls, model, etc.
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
      prompt: "Replace the 'x' with 'y'",
      initial_sketch: "let x = 5 in x",
      final_sketch: None,
    };
    let case2 = {
      prompt: "Replace the 'c' with 'a'",
      initial_sketch: "let c = 1 in c + c",
      final_sketch: None,
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
    | Collect;

  let can_undo = (action: t) => {
    switch (action) {
    | Init
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
    switch (action) {
    | Init =>
      print_endline("Here #0 : Init");
      // Create a new chat
      schedule_assistant_action(AssistantUpdateUtil.ChatAction(NewChat));
      // Create a new scratchpad
      schedule_editor_action(Editors.Update.Scratch(AddSlide));
      // Paste the initial sketch
      let a = Action.Paste(String(curr_case.initial_sketch));
      let perform_action = CodeEditable.Update.Perform(a);
      let cell_action = CellEditor.Update.MainEditor(perform_action);
      let scratch_action = Editors.Update.Scratch(CellAction(cell_action));
      schedule_editor_action(scratch_action);
      // Send the prompt to the assistant
      schedule_assistant_action(
        AssistantUpdateUtil.SendMessage(
          Composition(Request(curr_case.prompt)),
          None,
          assistant_model.current_chats.curr_composition_chat,
        ),
      );
      model |> Updated.return;
    // Await for assistant to complete
    | Collect =>
      print_endline("Here #7 : Collect");
      print_endline("Completed case. Todo: Decide what results to collect.");
      let new_cases = List.tl(model.cases);
      let new_model: Model.t = {
        cases: new_cases,
        curr_case: ListUtil.hd_opt(new_cases),
      };
      if (List.length(new_cases) > 0) {
        schedule_action(Init);
        new_model |> Updated.return;
      } else {
        model |> Updated.return;
      };
    };
  };
};
