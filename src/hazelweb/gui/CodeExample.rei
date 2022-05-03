open Prompt;
open Virtual_dom.Vdom;
let view:
  (
    ~inject: ModelAction.t => Ui_event.t,
    ~settings: Settings.t,
    ~font_metrics: FontMetrics.t,
    list(quest)
  ) =>
  Node.t;
