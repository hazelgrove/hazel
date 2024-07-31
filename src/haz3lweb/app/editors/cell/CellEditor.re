open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

/* A "Cell" with user-editable text at the top, and evaluation results at the bottom. */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    editor: CodeEditable.Model.t,
    result: EvalResult.Model.t,
  };

  let mk = editor => {
    editor: {
      editor,
      statics: CachedStatics.empty_statics,
    },
    result: EvalResult.Model.init,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = CodeEditable.Model.persistent;

  let persist = model => model.editor |> CodeEditable.Model.persist;
  let unpersist = pz => pz |> PersistentZipper.unpersist |> Editor.init |> mk;
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | MainEditor(CodeEditable.Update.t)
    | ResultAction(EvalResult.Update.t);

  let update = (~settings, action, model: Model.t) => {
    switch (action) {
    | MainEditor(action) =>
      let* editor =
        CodeEditable.Update.update(~settings, action, model.editor);
      {...model, editor};
    | ResultAction(action) =>
      let* result = EvalResult.Update.update(~settings, action, model.result);
      {...model, result};
    };
  };

  let calculate = (~settings, ~queue_worker, ~stitch, model: Model.t): Model.t => {
    let editor =
      CodeEditable.Update.calculate(~settings, ~stitch, model.editor);
    let result =
      EvalResult.Update.calculate(
        ~settings,
        ~queue_worker,
        editor |> CodeEditable.Model.get_statics,
        editor |> CodeEditable.Model.get_term,
        model.result,
      );
    {editor, result};
  };
};

module Selection = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | MainEditor
    | Result(EvalResult.Selection.t);

  let get_cursor_info = (~selection, model: Model.t): cursor(Update.t) => {
    switch (selection) {
    | MainEditor =>
      let+ ci =
        CodeEditable.Selection.get_cursor_info(~selection=(), model.editor);
      Update.MainEditor(ci);
    | Result(selection) =>
      let+ ci =
        EvalResult.Selection.get_cursor_info(~selection, model.result);
      Update.ResultAction(ci);
    };
  };

  let handle_key_event =
      (~selection, ~event, model: Model.t): option(Update.t) => {
    switch (selection) {
    | MainEditor =>
      CodeEditable.Selection.handle_key_event(
        ~selection=(),
        model.editor,
        event,
      )
      |> Option.map(x => Update.MainEditor(x))
    | Result(selection) =>
      EvalResult.Selection.handle_key_event(~selection, model.result, ~event)
      |> Option.map(x => Update.ResultAction(x))
    };
  };

  let jump_to_tile = (tile, model: Model.t): option((Update.t, t)) => {
    CodeEditable.Selection.jump_to_tile(tile, model.editor)
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
      EvalResult.View.view(
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
      ~attrs=[
        Attr.classes([
          "cell",
          Option.is_some(selected) ? "selected" : "deselected",
          locked ? "locked" : "unlocked",
        ]),
      ],
      Option.to_list(caption)
      @ [
        CodeEditable.View.view(
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
