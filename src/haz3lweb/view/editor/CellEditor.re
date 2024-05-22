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
      ~globals: Globals.t,
      ~select: Editors.Selection.cell => Ui_effect.t(unit),
      ~inject,
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
      ~globals,
      ~signal=
        fun
        | MakeActive(a) => select(a)
        | JumpTo(id) =>
          Effect.Many([
            select(MainEditor),
            inject(MainEditor(Jump(TileId(id)))),
          ]),
      ~inject=a => inject(ResultAction(a)),
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
        ~globals,
        ~signal=
          locked
            ? _ => Ui_effect.Ignore
            : fun
              | MakeActive => select(Editors.Selection.MainEditor),
        ~inject=
          locked
            ? _ => Ui_effect.Ignore : (action => inject(MainEditor(action))),
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
