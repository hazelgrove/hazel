open Haz3lcore;
open Util;

module CodeModel = CodeEditable.Model;
module Model = AssistantModel;

open AssistantUpdateBase;

let check_req =
    (
      ~schedule_action: t => unit,
      ~schedule_setting: AssistantSettings.action => unit,
      ~editor: CodeEditable.Model.t,
      ~chat_id: Id.t,
    )
    : unit => {
  let z = editor.editor.state.zipper;
  let caret = z.caret;
  let send_message = (tile_id, advanced_reasoning) => {
    schedule_setting(AssistantSettings.SwitchMode(CodeSuggestion));
    schedule_action(
      SendMessage(
        Completion(Request(tile_id, advanced_reasoning)),
        None,
        chat_id,
      ),
    );
  };

  // Check if user just typed ??
  switch (caret, Zipper.neighbor_tokens(z)) {
  | (Outer, (_, Some("??")))
  | (Outer, (Some("??"), _)) =>
    let tileId = Option.get(Indicated.index(z));
    let advanced_reasoning = false;
    send_message(tileId, advanced_reasoning);
  | (Outer, (_, Some("?a")))
  | (Outer, (Some("?a"), _)) =>
    let tileId = Option.get(Indicated.index(z));
    let advanced_reasoning = true;
    send_message(tileId, advanced_reasoning);
  | _ => ()
  };
};
