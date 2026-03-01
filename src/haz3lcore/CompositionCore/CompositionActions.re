open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

/* Semantic queries — extract information without modifying the editor */
[@deriving (show({with_path: false}), sexp, yojson)]
type language_server =
  | ShowUseSites(string)
  | ShowReferences(string);

/* Read actions — return information about the program without modifying it */
[@deriving (show({with_path: false}), sexp, yojson)]
type read_action =
  | GetSyntax(Action.Structural.path) /* Return pretty-printed code at path */
  | GetStatics(Action.Structural.path) /* Return type info at path */
  | GetContext(Action.Structural.path) /* Return in-scope bindings at path */
  | Select(string) /* Return focused syntax via selector language */
  | GetCompleteness; /* Report unfilled holes in the program */

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | EditorAction(Action.Structural.t)
  | ReadAction(read_action)
  | LanguageServerAction(language_server)
  | Initialize(string) /* replace entire program content (select-all + paste) */
  | WorkbenchAction(AgentWorkbench.Update.Action.BackendAction.action)
  | AgentContextAction(AgentContext.Update.action);
