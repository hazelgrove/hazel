open Virtual_dom;

let root_id: string;
let focus: unit => unit;

let basic_view: (~settings: Settings.t, ~width: int, UHExp.t) => Vdom.Node.t;

/**
 * Code representation of UHExp.
 */
let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~font_metrics: FontMetrics.t,
    ~settings: Settings.t,
    ~explanations: CodeExplanationSettings.t,
    ~doc_study: DocumentationStudySettings.t,
    ~cursor_inspector: CursorInspectorModel.t,
    Program.t
  ) =>
  Vdom.Node.t;
