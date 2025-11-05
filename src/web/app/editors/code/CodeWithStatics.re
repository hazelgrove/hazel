open Haz3lcore;
open Language;
open Util;
open WebUtil;

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
    dynamic_statics: Calc.saved((StaticsBase.Map.t, list(Id.t))),
  };

  let mk =
      (
        ~dynamics=Language.Dynamics.Map.empty,
        ~statics=CachedStatics.empty,
        editor,
      ) => {
    {
      editor,
      statics,
      dynamics,
      dynamic_statics: Calc.Pending,
    };
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

  let get_cursor_info = (model: t): Cursor.cursor(Action.t) => {
    let info =
      Indicated.ci_of(model.editor.state.zipper, model.statics.info_map);
    let dynamic_info =
      Indicated.ci_of(
        model.editor.state.zipper,
        model.statics.dynamic_info_map,
      );
    let id = Indicated.index(model.editor.state.zipper);
    {
      info,
      dynamic_info,
      dynamics:
        Option.bind(id, Language.Dynamics.Map.lookup(_, model.dynamics)),
      indicated_piece:
        Indicated.piece''(model.editor.state.zipper)
        |> Option.map(((p, _, _)) => p),
      selected_text:
        Some(
          () =>
            Printer.of_segment(model.editor.state.zipper.selection.content),
        ),
      selection: Some(model.editor.state.zipper.selection.content),
      editor: Some(model.editor),
      editor_read_only: true,
      editor_action: x => Some(x),
      undo_action: None,
      redo_action: None,
    };
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
        ~settings: Language.CoreSettings.t,
        ~is_edited,
        ~ctx=?,
        ~stitch,
        ~dynamics: Calc.t(Language.Dynamics.Map.t),
        ~is_dynamic_term,
        {editor, statics, dynamic_statics, dynamics: _}: Model.t,
      )
      : Model.t => {
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

    let ctx_init: Language.Ctx.t = Language.Builtins.ctx_init(Some(Int));
    // This should be a fold over the dynamics map getitng the type for each value

    let dynamic_statics =
      if (settings.dynamic_feedback) {
        Calc.Syntax.(
          dynamic_statics
          |> {
            let.calc dynamics = dynamics;

            // Get the current pinned call from the global cursor
            let pinned_call = Haz3lcore.ProbeProj.DynCursor.get_pinned_call();

            // Filter closures based on the pinned call
            let filtered_dynamics =
              Language.Dynamics.Map.filter_all_by_pin(pinned_call, dynamics);

            let dynamic_expressions: Id.Map.t(list(TermBase.exp_t)) =
              Id.Map.map(
                d => {
                  open Language;
                  let exps =
                    List.map((c: Dynamics.Probe.Closure.t) => c.value, d);
                  exps;
                },
                filtered_dynamics,
              );

            let dynamic_info_map =
              Language.Statics.mk(
                ~dynamics=dynamic_expressions,
                settings,
                ctx_init,
                statics.term,
              );

            let dynamic_error_ids =
              Language.StaticsBase.Map.error_ids(dynamic_info_map)
              |> List.filter(id => !List.mem(id, statics.error_ids));

            (dynamic_info_map, dynamic_error_ids);
          }
        );
      } else {
        Calc.set((Statics.Map.empty, []), dynamic_statics);
      };

    let statics: CachedStatics.t = {
      ...statics,
      dynamic_info_map: dynamic_statics |> Calc.get_value |> fst,
      dynamic_error_ids: dynamic_statics |> Calc.get_value |> snd,
    };

    let editor =
      Editor.Update.calculate(
        ~settings,
        ~is_edited=true,
        statics,
        Calc.get_value(dynamics),
        editor,
      );
    {
      editor,
      statics,
      dynamics: Calc.get_value(dynamics),
      dynamic_statics: Calc.save(dynamic_statics),
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
        (),
      );
    let statics_decos =
      Arms.Errors.of_ids(
        ~is_dynamic=false,
        ~font_metrics=globals.font_metrics,
        ~syntax=model.editor.syntax,
        model.statics.error_ids,
      );
    let dynamic_static_decos =
      Arms.Errors.of_ids(
        ~is_dynamic=true,
        ~font_metrics=globals.font_metrics,
        ~syntax=model.editor.syntax,
        model.statics.dynamic_error_ids,
      );
    div_c(
      "code-container",
      [code_text_view, statics_decos, dynamic_static_decos] @ overlays,
    );
  };
};
