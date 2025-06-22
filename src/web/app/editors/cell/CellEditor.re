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
  type persistent = CodeEditable.Model.persistent;

  let persist = model => model.editor |> CodeEditable.Model.persist;
  let to_string = model => model.editor |> CodeEditable.Model.to_string;
  let unpersist = (~settings as _, pz) =>
    pz |> PersistentZipper.unpersist |> Editor.of_zipper |> mk;
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

  let update = (~globals, action, model: Model.t) => {
    switch (action) {
    | MainEditor(action) =>
      let* editor =
        CodeEditable.Update.update(~globals, action, model.editor);
      {
        ...model,
        editor,
      };
    | ResultAction(action) =>
      let* result =
        EvalResult.Update.update(
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
      (~globals, ~queue_worker, ~stitch, {editor, result}: Model.t): Model.t => {
    let editor =
      CodeEditable.Update.calculate(
        ~globals,
        ~stitch,
        ~dynamics=EvalResult.Model.dynamics(result),
        ~is_dynamic_term=false,
        editor,
      );
    let result =
      EvalResult.Update.calculate(
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
        ~settings={
          ...globals.settings.core,
          assist: false,
        },
        ~queue_worker,
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
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | MainEditor(Editor.Focus.t)
    | Result(EvalResult.Selection.t);

  type event =
    | MakeActive(t);

  let get_cursor_info =
      (
        ~globals,
        ~inject,
        ~signal: event => Ui_effect.t(unit),
        ~selection,
        model: Model.t,
      )
      : Cursor.t => {
    switch (selection) {
    | MainEditor(f) =>
      CodeEditable.Focus.get_cursor_info(
        ~globals,
        ~read_only=false,
        ~take_focus=f => signal(MakeActive(MainEditor(f))),
        ~inject=x => inject(Update.MainEditor(x)),
        model.editor,
        f,
      )
    | Result(selection) =>
      EvalResult.Selection.get_cursor_info(
        ~globals,
        ~inject=x => inject(Update.ResultAction(x)),
        ~selection,
        model.result,
      )
    };
  };

  let jump_to_tile = (tile, model: Model.t): option((Update.t, t)) => {
    CodeEditable.Focus.jump_to_tile(tile, model.editor)
    |> Option.map(x =>
         (Update.MainEditor(x), MainEditor(Editor.Focus.here()))
       );
  };
};

module View = {
  let view =
      (
        ~globals: Globals.t,
        ~signal: Selection.event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Selection.t),
        ~caption: option(Node.t)=?,
        ~sort=Sort.Exp,
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
              signal(MakeActive(MainEditor(Editor.Focus.here()))),
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
      ~attrs=[Attr.classes(["cell", locked ? "locked" : "unlocked"])],
      Option.to_list(caption)
      @ [
        Editor.View.view(
          ~common={
            settings: globals.settings.core,
            font_metrics: globals.font_metrics,
            secondary_icons: globals.settings.secondary_icons,
            show_backpack_targets: globals.show_backpack_targets,
            color_highlights: globals.color_highlights,
            statics: model.editor.statics,
            dynamics: model.editor.dynamics,
          },
          ~mode=
            Editable({
              inject:
                locked
                  ? _ => Ui_effect.Ignore
                  : (a => inject(MainEditor(Perform(a)))),
              take_focus:
                locked
                  ? _ => Ui_effect.Ignore
                  : (f => signal(MakeActive(MainEditor(f)))),
              escape: _ => Ui_effect.Ignore,
              focus:
                switch (selected) {
                | Some(MainEditor(f)) => Some(f)
                | _ => None
                },
            }),
          ~overlays=overlays(model.editor.editor),
          ~sort,
          model.editor.editor,
        ),
      ]
      @ footer,
    );
  };
};
