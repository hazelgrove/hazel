open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

/* Semantic queries — extract information without modifying the editor */
[@deriving (show({with_path: false}), sexp, yojson)]
type language_server =
  | ShowUseSites(string)
  | ShowReferences(string);

[@deriving (show({with_path: false}), sexp, yojson)]
type probe_action =
  | PlaceProbe(list(string))
  | RemoveProbe(list(string))
  | ToggleProbe(list(string));

[@deriving (show({with_path: false}), sexp, yojson)]
type statics_action =
  | PlaceStatics(list(string))
  | RemoveStatics(list(string))
  | ToggleStatics(list(string));

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | EditorAction(Action.Structural.t)
  | LanguageServerAction(language_server)
  | Initialize(string) /* replace entire program content (select-all + paste) */
  | WorkbenchAction(AgentWorkbench.Update.Action.BackendAction.action)
  | AgentContextAction(AgentContext.Update.action)
  | ProbeAction(probe_action)
  | StaticsAction(statics_action);
