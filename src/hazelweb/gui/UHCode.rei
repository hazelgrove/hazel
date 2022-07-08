open Virtual_dom;

let root_id: string;
let focus: unit => unit;

let get_code_text_cells:
  (~settings: Settings.t, Program.t) => list(list(Vdom.Node.t));
/**
 * Code representation of UHExp.
 */
let view:
  (
    ~inject: ModelAction.t => Vdom.Event.t,
    ~font_metrics: FontMetrics.t,
    ~settings: Settings.t,
    ~cursor_inspector: CursorInspectorModel.t,
    Program.t,
    int,
    list(Vdom.Node.t)
  ) =>
  Vdom.Node.t;
