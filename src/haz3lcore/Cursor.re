type t = {
  info: option(Language.Info.t),
  contextual_actions: list(ContextualAction.t),
  current_projector: option(string),
};

let empty = {
  info: None,
  contextual_actions: [],
  current_projector: None,
};

let with_actions =
    (
      actions: list(ContextualAction.t),
      {info, contextual_actions, current_projector}: t,
    ) => {
  info,
  contextual_actions: contextual_actions @ actions,
  current_projector,
};

let with_actions_if =
    (
      condition: bool,
      actions: list(ContextualAction.t),
      {info, contextual_actions, current_projector}: t,
    ) =>
  if (condition) {
    with_actions(
      actions,
      {
        info,
        contextual_actions,
        current_projector,
      },
    );
  } else {
    {
      info,
      contextual_actions,
      current_projector,
    };
  };
