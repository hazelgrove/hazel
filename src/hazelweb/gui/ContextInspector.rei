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
    ~selected_instance_approach: option(
                                   (HoleInstance.t, Model.hole_inst_approach),
                                 ),
    ~settings: Settings.Evaluation.t,
    ~font_metrics: FontMetrics.t,
    Program.t
  ) =>
  Vdom.Node.t;
