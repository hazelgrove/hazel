open Util.WebUtil;
open Haz3lcore;

/* Read-only code viewer with statics, but no interaction. Notably,
   since there is no interaction, the user can see that there is an
   error but cannot select the error for more details. */

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Updated:
    editor: Editor.t,
    statics: CachedStatics.t,
    dynamics: Language.Dynamics.Map.t,
  };

  let mk = editor => {
    editor,
    statics: CachedStatics.empty,
    dynamics: Language.Dynamics.Map.empty,
  };

  let mk_from_exp =
      (
        ~settings: Language.CoreSettings.t,
        ~inline=false,
        term: Language.Exp.t,
      ) => {
    ExpToSegment.exp_to_segment(
      term,
      ~settings=ExpToSegment.Settings.of_core(~inline, settings),
    )
    |> Zipper.unzip
    |> Editor.Model.mk
    |> mk;
  };

  let get_statics = (model: t) => model.statics;

  let get_dynamics = (model: t) => model.dynamics;

  let get_cursor_info = (model: t): Cursor.cursor(Action.t) => {
    info: Indicated.ci_of(model.editor.state.zipper, model.statics.info_map),
    indicated_piece:
      Indicated.piece''(model.editor.state.zipper)
      |> Option.map(((p, _, _)) => p),
    selected_text:
      Some(() => Printer.to_string_selection(model.editor.state.zipper)),
    selection: Some(model.editor.state.zipper.selection.content),
    editor: Some(model.editor),
    editor_read_only: true,
    editor_action: x => Some(x),
    undo_action: None,
    redo_action: None,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = PersistentZipper.t;
  let persist = (model: t) =>
    model.editor.state.zipper |> PersistentZipper.persist;
  let to_string = (model: t) =>
    model.editor.state.zipper |> PersistentZipper.to_string;
  let unpersist = p =>
    p |> PersistentZipper.unpersist |> Editor.Model.mk |> mk;
};

module Update = {
  // There are no events for a read-only editor
  type t;

  /* Calculates the statics for the editor. */
  let calculate =
      (
        ~settings,
        ~is_edited,
        ~stitch,
        ~dynamics: Language.Dynamics.Map.t,
        ~is_dynamic_term,
        {editor, statics, dynamics: _}: Model.t,
      )
      : Model.t => {
    let statics =
      is_edited
        ? CachedStatics.init(
            ~settings,
            ~stitch,
            ~is_dynamic_term,
            editor.state.zipper,
          )
        : statics;
    let editor =
      Editor.Update.calculate(
        ~settings,
        ~is_edited,
        statics,
        dynamics,
        editor,
      );
    {
      editor,
      statics,
      dynamics,
    };
  };
};

module View = {
  // There are no events for a read-only editor
  type event;

  let view =
      (~globals, ~overlays: list(Node.t)=[], ~sort=Sort.root, model: Model.t) => {
    let {
      editor:
        {
          syntax: {measured, selection_ids, segment, shape_map, _},
          state: {zipper: z, _},
          _,
        },
      _,
    }: Model.t = model;
    let code_text_view =
      CodeViewable.view(
        ~globals,
        ~sort,
        ~measured,
        ~buffer_ids=Selection.is_buffer(z.selection) ? selection_ids : [],
        ~segment,
        ~shape_map,
      );
    let statics_decos = {
      module Deco =
        Deco.Deco({
          let globals = globals;
          let editor = model.editor;
          let statics = model.statics;
        });
      Deco.statics();
    };
    div_c("code-container", [code_text_view] @ statics_decos @ overlays);
  };
};
