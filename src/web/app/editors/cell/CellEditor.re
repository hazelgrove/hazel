open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

/* A "Cell" with user-editable text at the top, and evaluation results at the bottom. */
// This file follows conventions in [docs/ui-architecture.md]

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    editor: CodeEditable.Model.t,
    result: EvalResult.Model.t,
  };

  let mk = editor => {
    editor: {
      editor,
      statics: CachedStatics.empty,
      dynamics: Language.Dynamics.Map.empty,
    },
    result: EvalResult.Model.init,
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {
    editor: CodeEditable.Model.persistent,
    result: EvalResult.Model.persistent,
  };

  let persist = (model: t): persistent => {
    editor: model.editor |> CodeEditable.Model.persist,
    result: model.result |> EvalResult.Model.persist,
  };

  let unpersist = (~settings as _, {editor, result}: persistent, ~root): t => {
    editor: {
      editor: editor |> PersistentZipper.unpersist(~root) |> Editor.Model.mk,
      statics: CachedStatics.empty,
      dynamics: Language.Dynamics.Map.empty,
    },
    result: EvalResult.Model.unpersist(result),
  };

  let to_string = (model: t) => model.editor |> CodeEditable.Model.to_string;
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | MainEditor(CodeEditable.Update.t)
    | ResultAction(EvalResult.Update.t);

  let can_undo = (action: t) => {
    switch (action) {
    | MainEditor(action) => CodeEditable.Update.can_undo(action)
    | ResultAction(action) => EvalResult.Update.can_undo(action)
    };
  };

  let update = (~settings, action, model: Model.t) => {
    switch (action) {
    | MainEditor(action) =>
      let* editor =
        CodeEditable.Update.update(~settings, action, model.editor);
      {
        ...model,
        editor,
      };
    | ResultAction(action) =>
      let* result =
        EvalResult.Update.update(
          ~settings={
            ...settings,
            core: {
              ...settings.core,
              assist: false,
            },
          },
          action,
          model.result,
        );
      {
        ...model,
        result,
      };
    };
  };

  let calculate =
      (
        ~settings,
        ~is_edited,
        ~queue_worker,
        ~stitch,
        {editor, result}: Model.t,
      )
      : Model.t => {
    let editor =
      CodeEditable.Update.calculate(
        ~settings,
        ~is_edited,
        ~stitch,
        ~dynamics=EvalResult.Model.dynamics(result),
        ~is_dynamic_term=false,
        editor,
      );
    let result =
      EvalResult.Update.calculate(
        ~settings={
          ...settings,
          assist: false,
        },
        ~queue_worker,
        ~is_edited,
        editor |> CodeEditable.Model.get_statics,
        result,
      );
    {
      editor,
      result,
    };
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
        ~globals={
          ...globals,
          settings: {
            ...globals.settings,
            core: {
              ...globals.settings.core,
              assist: false,
            },
          },
        },
        ~signal=
          fun
          | MakeActive(a) => signal(MakeActive(Result(a)))
          | JumpTo(id) =>
            Effect.Many([
              signal(MakeActive(MainEditor)),
              inject(MainEditor(Perform(Move(Goal(TileId(id)))))),
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
        ~root=model.editor.editor.state.zipper.root,
        model.result,
      );
    div(
      ~attrs=[Attr.classes(["cell", locked ? "locked" : "unlocked"])],
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
