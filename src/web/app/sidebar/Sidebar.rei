/* The sidebar shell. Page composes this; the per-panel view helpers and
   collapse plumbing are private. */

let view:
  (
    ~globals: Globals.t,
    ~cursor: Cursor.cursor(Editors.Update.t),
    ~explain_this_inject: ExplainThisUpdate.update => Ui_effect.t(unit),
    ~explainThisModel: ExplainThisModel.t,
    ~log_model: LogSidebar.Model.t,
    ~log_count: int,
    ~editors_inject: Editors.Update.t => Ui_effect.t(unit),
    ~editors: Editors.Model.t,
    ~selection: Editors.Selection.t,
    ~editor: CodeWithStatics.Model.t,
    ~problem_editors:
      list((option(string), list(CodeWithStatics.Model.t))),
    ~signal: Editors.View.signal => Ui_effect.t(unit)
  ) =>
  Virtual_dom.Vdom.Node.t;
