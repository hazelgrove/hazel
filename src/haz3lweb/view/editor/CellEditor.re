open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

type model = {
  editor: CodeEditor.model,
  result: ModelResult.t,
};

type action =
  | MainEditor(CodeEditor.action)
  | ResultAction(CellResult.action);

let update = ReadOnlyEditor.update;

let calculate = ReadOnlyEditor.calculate;

type event = CodeEditor.event;

let view =
    (
      ~select: Editors.Selection.cell => Ui_effect.t(unit),
      ~inject,
      ~inject_global,
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~highlights: option(ColorSteps.colorMap),
      ~selected: option(Editors.Selection.cell),
      ~caption: option(Node.t)=?,
      ~sort=?,
      ~result_kind=?,
      ~locked=false,
      model,
    ) => {
  let (footer, overlays) =
    CellResult.view(
      ~signal=
        fun
        | MakeActive(a) => select(a)
        | MouseUp => inject_global(Update.SetMeta(Mouseup))
        | MouseDown => inject_global(Update.SetMeta(Mousedown)),
      ~inject=a => inject(ResultAction(a)),
      ~inject_global,
      ~ui_state,
      ~settings,
      ~selected=selected == Some(Result(0)),
      ~result_kind?,
      ~locked,
      model.result,
    );
  div(
    ~attr=
      Attr.classes([
        "cell",
        Option.is_some(selected) ? "selected" : "deselected",
        locked ? "locked" : "unlocked",
      ]),
    Option.to_list(caption)
    @ [
      CodeEditor.view(
        ~signal=
          locked
            ? _ => Ui_effect.Ignore
            : fun
              | MouseUp => inject_global(Update.SetMeta(Mouseup))
              | MouseDown => inject_global(Update.SetMeta(Mousedown))
              | MakeActive => select(Editors.Selection.MainEditor),
        ~inject=
          locked
            ? _ => Ui_effect.Ignore
            : (action => inject_global(PerformAction(action))),
        ~ui_state,
        ~settings,
        ~selected=selected == Some(MainEditor),
        ~highlights,
        ~overlays=overlays(model.editor.editor),
        ~sort?,
        model.editor,
      ),
    ]
    @ footer,
  );
};
