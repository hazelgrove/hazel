[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | EditorAction(Action.agent_editor_action)
  | WorkbenchAction(AgentWorkbench.Update.Action.BackendAction.action)
  | AgentContextAction(AgentContext.Update.action);
