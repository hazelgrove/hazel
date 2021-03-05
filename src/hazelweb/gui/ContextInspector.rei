open Virtual_dom;

/**
 * Raised when a selected hole instance does not exist.
 * Indicates a bug.
 */
exception InvalidInstance;

/**
 * Panel showing the typing context and environment at the cursor.
 */
let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~selected_instance: option(TaggedNodeInstance.t),
    ~settings: Settings.Evaluation.t,
    ~livelit_ctx_open: bool,
    ~typing_ctx_open: bool,
    Program.t
  ) =>
  Vdom.Node.t;
