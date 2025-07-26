open Virtual_dom.Vdom;

// Assistant modes are separated into their own modules
// This allows for mode-specific logic, namely update actions,
// to be separated from one another.
module type AssistantMode = {
  /* The internal model type of the assistant mode which will
   * be serialized and persisted. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  /* An internal action type to be used in actions which
   * update the model. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
  let init: unit => model;
  let update: (action, model) => model;
  let view: (model, action => unit) => Node.t;
};
