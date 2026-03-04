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
    editor: Editor.t,
    context_menu: context_menu_state,
    statics: CachedStatics.t,
    dynamics: Language.Dynamics.Map.t,
  };

  let context_menu_is_open = (model: t): bool => model.context_menu != None;

  let mk =
      (
        ~dynamics=Language.Dynamics.Map.empty,
        ~statics=CachedStatics.empty,
        editor,
      ) => {
    editor,
    statics,
    dynamics,
    context_menu: None,
  };

  let mk_from_exp =
      (
        ~settings: Language.CoreSettings.t,
        ~inline=Inline.Block,
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
    error_ids: model.statics.error_ids,
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

type statics_mode =
  | StaticsNormal
  | StaticsDefer
  | StaticsForce;

/* Debounce statics computation during rapid typing. Only one mode is
   active at a time, so a single timer/flag is shared across all modes. */
module StaticsDebounce = {
  let debounce_ms = 225.0;
  let timer_id: ref(option(Js_of_ocaml.Dom_html.timeout_id)) = ref(None);
  let force_on_next: ref(bool) = ref(false);

  /* Call from calculate to get the statics_mode for this cycle.
     schedule_refresh should dispatch the mode's RefreshStatics action. */
  let consume = (~is_edited, ~schedule_refresh: unit => unit): statics_mode => {
    let force_now = force_on_next^;
    force_on_next := false;
    if (is_edited && debounce_ms > 0.0) {
      switch (timer_id^) {
      | Some(id) => Js_of_ocaml.Dom_html.window##clearTimeout(id)
      | None => ()
      };
      timer_id :=
        Some(
          Js_of_ocaml.Dom_html.window##setTimeout(
            Js_of_ocaml.Js.wrap_callback(() => {
              timer_id := None;
              schedule_refresh();
            }),
            debounce_ms,
          ),
        );
      StaticsDefer;
    } else if (force_now) {
      StaticsForce;
    } else {
      StaticsNormal;
    };
  };
};

module Update = {
  // There are no events for a read-only editor
  type t;

  /* Calculates the statics for the editor. */
  let calculate =
      (
        ~settings,
        ~is_edited,
        ~statics_mode=StaticsNormal,
        ~ctx=?,
        ~stitch,
        ~dynamics: Language.Dynamics.Map.t,
        ~is_dynamic_term,
        ~ana=?,
        {editor, statics, context_menu, dynamics: _}: Model.t,
      )
      : Model.t => {
    let editor =
      Editor.Update.calculate(
        ~settings,
        ~is_edited,
        statics,
        dynamics,
        editor,
      );
    let statics =
      statics_mode == StaticsForce || is_edited && statics_mode != StaticsDefer
        ? CachedStatics.init(
            ~settings,
            ~stitch,
            ~ctx?,
            ~ana?,
            ~is_dynamic_term,
            editor.state.zipper,
          )
        : statics;
    /* Rebuild shape map with fresh statics: projector placeholder sizes
       may depend on statics (e.g. livelit projectors look up their size
       from the context), but the shape map above was built with stale
       statics. Rebuild it now that statics are fresh. */
    let editor =
      if (is_edited) {
        let projector_shapes =
          ProjectorInfo.ShapeMapSemantics.mk(
            editor.syntax.projectors,
            editor.state.zipper.refractors,
            statics.info_map,
            dynamics,
          );
        let refractor_shape_map = Id.Map.empty;
        let measured =
          Measured.of_segment(
            editor.syntax.segment,
            projector_shapes,
            refractor_shape_map,
          );
        Editor.Model.{
          ...editor,
          syntax: {
            ...editor.syntax,
            shape_map: projector_shapes,
            measured,
          },
        };
      } else {
        editor;
      };
    {
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
