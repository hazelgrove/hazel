open Util.Web;
open Haz3lcore;

/* Read-only code viewer with statics, but no interaction. Notably,
   since there is no interaction, the user can see that there is an
   error but cannot select the error for more details. */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Updated:
    editor: Editor.t,
    // Calculated:
    statics: CachedStatics.t,
  };

  let mk = editor => {editor, statics: CachedStatics.empty};

  let mk_from_exp = (~settings: CoreSettings.t, ~inline=false, term: Exp.t) => {
    ExpToSegment.exp_to_segment(
      term,
      ~settings=ExpToSegment.Settings.of_core(~inline, settings),
    )
    |> Zipper.unzip
    |> Editor.Model.mk
    |> mk;
  };

  let get_statics = (model: t) => model.statics;

  let get_cursor_info = (model: t): Cursor.cursor(Action.t) => {
    info: Indicated.ci_of(model.editor.state.zipper, model.statics.info_map),
    selected_text:
      Some(Printer.to_string_selection(model.editor.state.zipper)),
    editor: Some(model.editor),
    editor_action: x => Some(x),
    undo_action: None,
    redo_action: None,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = PersistentZipper.t;
  let persist = (model: t) =>
    model.editor.state.zipper |> PersistentZipper.persist;
  let unpersist = p =>
    p |> PersistentZipper.unpersist |> Editor.Model.mk |> mk;
};

module Update = {
  // There are no events for a read-only editor
  type t;

  /* Calculates the statics for the editor. */
  let calculate =
      (~settings, ~is_edited, ~stitch, {editor, statics: _}: Model.t)
      : Model.t => {
    let statics = CachedStatics.init(~settings, ~stitch, editor.state.zipper);
    let editor =
      Editor.Update.calculate(~settings, ~is_edited, statics, editor);
    {editor, statics};
  };
};

module View = {
  // There are no events for a read-only editor
  type event;

  let view =
      (~globals, ~overlays: list(Node.t)=[], ~sort=Sort.root, model: Model.t) => {
    let {
      statics: {info_map, _},
      editor: {syntax: {measured, selection_ids, segment, holes, _}, _},
      _,
    }: Model.t = model;
    let code_text_view =
      CodeViewable.view(
        ~globals,
        ~sort,
        ~measured,
        ~buffer_ids=selection_ids,
        ~segment,
        ~holes,
        ~info_map,
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
