[@deriving (show({with_path: false}), sexp, yojson)]
type selection = Editors.Selection.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  globals: Globals.Model.t,
  editors: Editors.Model.t,
  explain_this: ExplainThisModel.t,
  assistant: AssistantModel.t,
  selection,
};

let equal = (===);
