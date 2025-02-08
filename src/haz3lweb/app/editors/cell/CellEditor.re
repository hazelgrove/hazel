open Virtual_dom.Vdom;
open Node;

/* A "Cell" with user-editable text at the top, and evaluation results at the bottom. */
// This file follows conventions in [docs/ui-architecture.md]

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    editor: EditorManager.Model.t,
    result: EvalResult.Model.t,
  };

  let mk = editor => {editor, result: EvalResult.Model.init};

  let mk_from_manager = editor => {editor, result: EvalResult.Model.init};

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = EditorManager.Model.persistent;

  let persist = model => model.editor |> EditorManager.Model.persist;
  let unpersist = pz => EditorManager.Model.unpersist(pz) |> mk_from_manager;
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | MainEditor(EditorManager.Update.t)
    | ResultAction(EvalResult.Update.t);

  let update = (~settings: Settings.t, action, model: Model.t) => {
    switch (action) {
    | MainEditor(action) =>
      let* editor =
        EditorManager.Update.update(
          ~settings=settings.core,
          action,
          model.editor,
        );
      {...model, editor};
    | ResultAction(action) =>
      let* result =
        EvalResult.Update.update(
          ~settings={...settings.core, assist: false},
          action,
          model.result,
        );
      {...model, result};
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
      EditorManager.Update.calculate(
        ~settings,
        ~is_edited,
        ~stitch,
        ~dynamics=EvalResult.Model.dynamics(result),
        editor,
      );
    let result =
      EvalResult.Update.calculate(
        ~settings={...settings, assist: false},
        ~queue_worker,
        ~is_edited,
        editor.statics,
        result,
      );
    {editor, result};
  };
};

module Selection = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | MainEditor(EditorManager.Focus.t)
    | Result(EvalResult.Selection.t);

  let get_cursor_info = (~selection, model: Model.t): cursor(Update.t) => {
    switch (selection) {
    | MainEditor(focus) =>
      let+ ci =
        EditorManager.Focus.get_cursor_info(~selection=focus, model.editor);
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
    | MainEditor(focus) =>
      EditorManager.Focus.handle_key_event(
        ~selection=focus,
        ~event,
        model.editor,
      )
      |> Option.map(x => Update.MainEditor(x))
    | Result(selection) =>
      EvalResult.Selection.handle_key_event(~selection, model.result, ~event)
      |> Option.map(x => Update.ResultAction(x))
    };
  };

  let jump_to_tile = (tile, model: Model.t): option((Update.t, t)) => {
    EditorManager.Focus.jump_to_tile(tile, model.editor)
    |> Option.map(((x, y)) => (Update.MainEditor(x), MainEditor(y)));
  };

  let default_selection = (model: Model.t) =>
    MainEditor(EditorManager.Focus.default_selection(model.editor));
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
          | JumpTo(id) => globals.inject_global(JumpToTile(id)),
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
      ~attrs=[Attr.classes(["cell", locked ? "locked" : "unlocked"])],
      Option.to_list(caption)
      @ [
        EditorManager.View.view(
          ~dynamics=EvalResult.Model.dynamics(model.result),
          ~globals,
          ~signal=
            locked
              ? _ => Ui_effect.Ignore
              : fun
                | Focus(s) => signal(MakeActive(MainEditor(s))),
          ~inject=
            locked
              ? _ => Ui_effect.Ignore
              : (action => inject(MainEditor(action))),
          ~selected=
            switch (selected) {
            | Some(MainEditor(s)) => Some(s)
            | _ => None
            },
          ~overlays=
            overlays(model.editor |> EditorManager.Model.get_root_editor),
          model.editor,
        )
        |> Haz3lcore.ProjectorBase.View.get_tylr,
      ]
      @ footer,
    );
  };
};
