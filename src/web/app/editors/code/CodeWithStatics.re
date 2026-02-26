open Util;
open Util.WebUtil;
open Haz3lcore;

/* Read-only code viewer with statics, but no interaction. Notably,
   since there is no interaction, the user can see that there is an
   error but cannot select the error for more details. */

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  /* Context menu state: None = closed, Some(n) = open with item n selected */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type context_menu_state = option(int);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Updated:
    editor: Calc.t(Editor.t),
    is_edited: bool,
    context_menu: context_menu_state,
    statics: Calc.saved(CachedStatics.t),
    dynamics: Language.Dynamics.Map.t,
  };

  let context_menu_is_open = (model: t): bool => model.context_menu != None;

  let mk =
      (~dynamics=Language.Dynamics.Map.empty, ~statics=Calc.Pending, editor) => {
    editor: Calc.NewValue(editor),
    is_edited: true, // so that it recalcualtes fully
    statics,
    dynamics,
    context_menu: None,
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

  let get_editor = (model: t): Editor.t => model.editor |> Calc.get_value;

  let stale_editor = (model: t): t => {
    ...model,
    editor: model.editor |> Calc.make_new,
  };

  let get_zipper = (model: t) => (model |> get_editor).state.zipper;

  let get_statics = (model: t) =>
    model.statics |> Calc.get_saved(CachedStatics.empty);

  let get_dynamics = (model: t) => model.dynamics;

  let get_cursor_info = (model: t): Cursor.cursor(Action.t) => {
    let editor = model |> get_editor;
    let statics = model.statics |> Calc.get_saved(CachedStatics.empty);
    {
      info: Indicated.ci_of(editor.state.zipper, statics.info_map),
      indicated_piece:
        Indicated.piece''(editor.state.zipper)
        |> Option.map(((p, _, _)) => p),
      selected_text:
        Some(
          () =>
            Printer.of_segment(
              ~refractors=editor.state.zipper.refractors.manuals,
              editor.state.zipper.selection.content,
            ),
        ),
      selection: Some(editor.state.zipper.selection.content),
      editor: Some(editor),
      editor_read_only: true,
      editor_action: x => Some(x),
      undo_action: None,
      redo_action: None,
      error_ids: statics.error_ids,
    };
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = PersistentZipper.t;
  let persist = (model: t) => model |> get_zipper |> PersistentZipper.persist;
  let to_string = (model: t) =>
    model |> get_zipper |> PersistentZipper.to_string;
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
        ~ctx=?,
        ~stitch,
        ~dynamics: Language.Dynamics.Map.t,
        ~is_dynamic_term,
        ~ana=?,
        {editor, statics, is_edited, context_menu, dynamics: _}: Model.t,
      )
      : Model.t => {
    let editor =
      editor
      |> {
        open Calc.Syntax;
        let.calc_t editor = editor
        and.calc settings = settings;
        Editor.Update.calculate(
          ~settings,
          ~is_edited,
          statics |> Calc.get_saved(CachedStatics.empty),
          dynamics,
          editor,
        );
      };

    let statics =
      statics
      |> {
        open Calc.Syntax;
        let.calc editor = editor
        and.calc settings = settings;
        CachedStatics.init(
          ~settings,
          ~stitch,
          ~ctx?,
          ~ana?,
          ~is_dynamic_term,
          editor.state.zipper,
        );
      };

    let editor = Calc.make_old(editor);
    let statics = Calc.save(statics);

    {
      editor,
      statics,
      is_edited: false,
      dynamics,
      context_menu,
    };
  };
};

module View = {
  // There are no events for a read-only editor
  type event;

  let view = (~globals, ~overlays: list(Node.t)=[], model: Model.t) => {
    let editor: Editor.t = model.editor |> Calc.get_value;
    let statics: CachedStatics.t =
      model.statics |> Calc.get_saved(CachedStatics.empty);
    let {
      syntax: {measured, selection_ids, segment, shape_map, term_data, _},
      state: {zipper: z, _},
      _,
    }: Editor.t =
      model.editor |> Calc.get_value;
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
    let error_decos =
      Arms.Errors.of_ids(
        ~font_metrics=globals.font_metrics,
        ~syntax=editor.syntax,
        statics.error_ids,
      );
    let warning_ids =
      globals.settings.core.display_warnings ? statics.warning_ids : [];
    let warning_decos =
      Arms.Errors.of_ids(
        ~is_warning=true,
        ~font_metrics=globals.font_metrics,
        ~syntax=editor.syntax,
        warning_ids,
      );
    let container_classes =
      ["code-container"] @ (globals.meta_down ? ["meta-down"] : []);
    Node.div(
      ~attrs=[Attr.classes(container_classes)],
      // errors after warnings to prioritize errors over warnings
      [code_text_view, warning_decos, error_decos] @ overlays,
    );
  };
};
