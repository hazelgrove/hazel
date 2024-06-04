open Virtual_dom.Vdom;
open Haz3lcore;
open Sexplib.Std;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Updated:
    editor: Editor.t,
    // Calculated:
    statics: CachedStatics.statics,
  };

  let mk = editor => {editor, statics: CachedStatics.empty_statics};

  let mk_from_editor = (~settings, ~stitch, editor: Editor.t) => {
    let term =
      MakeTerm.from_zip_for_sem(editor.state.zipper) |> fst |> stitch;
    let info_map =
      Interface.Statics.mk_map_ctx(settings, Builtins.ctx_init, term);
    let error_ids =
      Statics.Map.error_ids(editor.state.meta.term_ranges, info_map);
    {
      editor,
      statics: {
        term,
        info_map,
        error_ids,
      },
    };
  };

  let mk_from_exp = (~inline=false, term: Exp.t) => {
    ExpToSegment.exp_to_editor(term, ~inline)
    |> mk_from_editor(~stitch=x => x);
  };

  let get_elab = model => model.statics.term;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = PersistentZipper.t;
  let persist = model => model.editor.state.zipper |> PersistentZipper.persist;
  let unpersist = p =>
    p |> PersistentZipper.unpersist |> Editor.init |> mk_from_editor;
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Perform(Action.t)
    | Undo
    | Redo
    | Reparse
    | Assistant(UpdateAssistant.t)
    | DebugConsole(string);

  exception CantReset;

  let update =
      (~settings: Settings.t, action: t, model: Model.t): Updated.t(Model.t) => {
    let perform = (action, model: Model.t) =>
      Perform.go(~settings=settings.core, action, model.editor)
      |> (
        fun
        | Ok(editor) => Model.{editor, statics: model.statics}
        | Error(err) => raise(Action.Failure.Exception(err))
      );
    let perform_all =
      List.fold_left((model, action) => perform(action, model));
    switch (action) {
    | Perform(action) =>
      perform(action, model)
      |> Updated.return(
           ~is_edit=Action.is_edit(action),
           ~recalculate=Action.is_edit(action),
           ~scroll_active={
             switch (action) {
             | Move(_)
             | MoveToNextHole(_)
             | Jump(_)
             | Select(Resize(_) | Term(_) | Smart | Tile(_))
             | Destruct(_)
             | Insert(_)
             | Pick_up
             | Put_down
             | RotateBackpack
             | MoveToBackpackTarget(_)
             | Paste(_) => true
             | Unselect(_)
             | Select(All)
             | Suggest(_)
             | ResetSuggestion => false
             };
           },
         )
    | Undo =>
      switch (Editor.undo(model.editor)) {
      | Some(editor) =>
        Model.{editor, statics: model.statics} |> Updated.return
      | None => model |> Updated.return_quiet
      }
    | Redo =>
      switch (Editor.redo(model.editor)) {
      | Some(editor) =>
        Model.{editor, statics: model.statics} |> Updated.return
      | None => model |> Updated.return_quiet
      }
    | Reparse =>
      let zipper_init = Zipper.init();
      let ed_str = Printer.to_string_editor(model.editor);
      switch (Printer.zipper_of_string(~zipper_init, ed_str)) {
      | None => raise(CantReset)
      | Some(z) =>
        //TODO: add correct action to history (Pick_up is wrong)
        let* editor =
          Haz3lcore.Editor.new_state(Pick_up, z, model.editor)
          |> Updated.return;
        Model.{editor, statics: model.statics};
      };
    | Assistant(a) =>
      perform_all(
        model,
        UpdateAssistant.assistant_action_to_editor_actions(
          ~settings=settings.core,
          model.editor,
          a,
        ),
      )
      |> (
        switch (a) {
        | Prompt(_) => Updated.return_quiet(_)
        | AcceptSuggestion => Updated.return(_)
        }
      )
    | DebugConsole(key) =>
      DebugConsole.print(~settings, model.editor, key);
      model |> Updated.return_quiet;
    };
  };

  let calculate = (~settings, ~stitch, model: Model.t): Model.t =>
    if (DHExp.fast_equal(
          MakeTerm.from_zip_for_sem(model.editor.state.zipper) |> fst,
          model.statics.term,
        )) {
      model;
    } else {
      Model.mk_from_editor(~settings, ~stitch, model.editor);
    };
};

module View = {
  // There are no events for a read-only editor
  type event;

  let view =
      (~globals, ~overlays: list(Node.t)=[], ~sort=Sort.root, model: Model.t) => {
    let code_text_view = Code.view(~globals, ~sort, model.editor);
    let statics_decos = {
      module Deco =
        Deco.Deco({
          let globals = globals;
          let editor = model.editor;
        });
      Deco.statics(model.statics.error_ids);
    };
    Node.div(
      ~attr=Attr.many([Attr.classes(["code-container"])]),
      [code_text_view] @ statics_decos @ overlays,
    );
  };
};
