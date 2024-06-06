open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    editor: CodeEditor.Model.t,
    result: Result.Model.t,
  };

  let mk = editor => {
    editor: {
      editor,
      statics: CachedStatics.empty_statics,
    },
    result: Result.Model.init,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = CodeEditor.Model.persistent;

  let persist = model => model.editor |> CodeEditor.Model.persist;
  let unpersist = pz => pz |> PersistentZipper.unpersist |> Editor.init |> mk;
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | MainEditor(CodeEditor.Update.t)
    | ResultAction(Result.Update.t);

  let update = (~settings, action, model: Model.t) => {
    switch (action) {
    | MainEditor(action) =>
      let* editor = CodeEditor.Update.update(~settings, action, model.editor);
      {...model, editor};
    | ResultAction(action) =>
      let* result = Result.Update.update(~settings, action, model.result);
      {...model, result};
    };
  };

  let calculate =
      (~settings, ~schedule_action, ~stitch, ~immediate=false, model: Model.t)
      : Model.t => {
    let editor =
      CodeEditor.Update.calculate(~settings, ~stitch, model.editor);
    let result =
      Result.Update.calculate(
        ~settings,
        ~schedule_action=a => schedule_action(ResultAction(a)),
        ~immediate,
        editor |> CodeEditor.Model.get_elab,
        model.result,
      );
    {editor, result};
  };
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | MainEditor
    | Result(Result.Selection.t);

  let get_cursor_info = (~selection, model: Model.t) => {
    switch (selection) {
    | MainEditor => CodeEditor.Selection.get_cursor_info(model.editor)
    | Result(selection) =>
      Result.Selection.get_cursor_info(~selection, model.result)
    };
  };

  let handle_key_event =
      (~selection, ~event, model: Model.t): option(Update.t) => {
    switch (selection) {
    | MainEditor =>
      CodeEditor.Selection.handle_key_event(model.editor, event)
      |> Option.map(x => Update.MainEditor(x))
    | Result(selection) =>
      Result.Selection.handle_key_event(~selection, model.result, ~event)
      |> Option.map(x => Update.ResultAction(x))
    };
  };

  let jump_to_tile = (tile, model: Model.t): option((Update.t, t)) => {
    CodeEditor.Selection.jump_to_tile(tile, model.editor)
    |> Option.map(x => (Update.MainEditor(x), MainEditor));
  };
};

module View = {
  type event =
    | MakeActive(Selection.t);

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Selection.t),
        ~caption: option(Node.t)=?,
        ~sort=?,
        ~result_kind=?,
        ~locked=false,
        model: Model.t,
      ) => {
    let (footer, overlays) =
      Result.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(a) => signal(MakeActive(Result(a)))
          | JumpTo(id) =>
            Effect.Many([
              signal(MakeActive(MainEditor)),
              inject(MainEditor(Perform(Jump(TileId(id))))),
            ]),
        ~inject=a => inject(ResultAction(a)),
        ~selected={
          switch (selected) {
          | Some(Result(a)) => Some(a)
          | _ => None
          };
        },
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
                | MakeActive => signal(MakeActive(MainEditor)),
          ~inject=
            locked
              ? _ => Ui_effect.Ignore
              : (action => inject(MainEditor(action))),
          ~selected=selected == Some(MainEditor),
          ~overlays=overlays(model.editor.editor),
          ~sort?,
          model.editor,
        ),
      ]
      @ footer,
    );
  };
};
