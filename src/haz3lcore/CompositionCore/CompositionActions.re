open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;
open Language;

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
  | GetCanonical(string) /* Return canonical (numeric + named) selectors for node */
  | GetCompleteness /* Report unfilled holes in the program */
  | SelectorGetStatics(string) /* Return type info at selector-resolved node */
  | SelectorGetContext(string); /* Return in-scope bindings at selector-resolved node */

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
type syntax_projector_action =
  | PlaceSyntaxProjector(ProjectorKind.t, list(string))
  | RemoveSyntaxProjector(list(string))
  | ToggleSyntaxProjector(ProjectorKind.t, list(string));

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | EditorAction(Action.Structural.t)
  | ReadAction(read_action)
  | LanguageServerAction(language_server)
  | InsertAtProgramBoundary(Action.Structural.insert_target, string) /* no-path insert: prepend (Before) or append (After) to the whole program */
  | WorkbenchAction(AgentWorkbench.Update.Action.BackendAction.action)
  | AgentContextAction(AgentContext.Update.action)
  | ProbeAction(probe_action)
  | StaticsAction(statics_action)
  | SyntaxProjectorAction(syntax_projector_action);
