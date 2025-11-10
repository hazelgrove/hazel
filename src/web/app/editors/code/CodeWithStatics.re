open Util;
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
    context_menu: bool,
    statics: CachedStatics.t,
    dynamics: Language.Dynamics.Map.t,
  };

  let mk =
      (
        ~dynamics=Language.Dynamics.Map.empty,
        ~statics=CachedStatics.empty,
        editor,
      ) => {
    editor,
    statics,
    dynamics,
    context_menu: false,
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
      Some(
        () =>
          Printer.of_segment(
            ~refractors=model.editor.state.zipper.refractors.manuals,
            model.editor.state.zipper.selection.content,
          ),
      ),
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
        ~ctx=?,
        ~stitch,
        ~dynamics: Language.Dynamics.Map.t,
        ~is_dynamic_term,
        {editor, statics, context_menu, _}: Model.t,
      )
      : Model.t => {
    //TODO(andrew): resolve this cycle
    // might be problematic not to calc editor again below...
    let editor =
      Editor.Update.calculate(
        ~settings,
        ~is_edited,
        statics,
        dynamics,
        editor,
      );
    let statics =
      is_edited
        ? CachedStatics.init(
            ~settings,
            ~stitch,
            ~ctx?,
            ~is_dynamic_term,
            editor.state.zipper,
          )
        : statics;
    {
      // let editor =
      //   Editor.Update.calculate(
      //     ~settings,
      //     ~is_edited,
      //     statics,
      //     dynamics,
      //     editor,
      //   );

      editor,
      statics,
      dynamics,
      context_menu,
    };
  };
};

module View = {
  // There are no events for a read-only editor
  type event;

  let view = (~globals, ~overlays: list(Node.t)=[], model: Model.t) => {
    let {
      editor:
        {
          syntax: {measured, selection_ids, segment, shape_map, term_data, _},
          state: {zipper: z, _},
          _,
        },
      _,
    }: Model.t = model;
    let code_text_view =
      CodeViewable.view(
        ~globals,
        ~measured,
        ~term_data,
        ~buffer_ids=Selection.is_buffer(z.selection) ? selection_ids : [],
        ~segment,
        ~shape_map,
        ~refractor_shape_map=Id.Map.empty //Id.Map.map(_ => 2, z.refractors.map),
      );
    let statics_decos =
      Arms.Errors.of_ids(
        ~font_metrics=globals.font_metrics,
        ~syntax=model.editor.syntax,
        model.statics.error_ids,
      );
    let container_classes =
      ["code-container"] @ (globals.meta_down ? ["meta-down"] : []);
    Node.div(
      ~attrs=[Attr.classes(container_classes)],
      [code_text_view, statics_decos] @ overlays,
    );
  };
};
