open Haz3lcore;
open AssistantModel;

module Update = {
  /*
   A fairly thin wrapper around the AssistantUpdate module
   */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = AssistantUpdateAction.t;
  type model = AssistantModel.t;

  let update =
      (
        ~settings,
        ~model: model,
        ~action: t,
        ~editor: CodeEditable.Model.t,
        ~schedule_action,
        ~schedule_editor_action,
      ) => {
    let zipper = editor.editor.state.zipper;
    let info_map = editor.statics.info_map;
    AssistantUpdate.update(
      ~settings,
      ~action,
      ~model,
      ~zipper,
      ~info_map,
      ~schedule_action,
      ~schedule_editor_action,
    )
    |> Updated.return_quiet;
  };
};

module Store =
  Store.F({
    [@deriving (show({with_path: false}), yojson, sexp)]
    type t = model;
    let default = () => null_model();

    let key = Store.Assistant;
  });
