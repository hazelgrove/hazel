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
    dynamics: Dynamics.t,
    dynamic_statics: Calc.saved((StaticsBase.Map.t, list(Id.t))),
    pinned_call: Calc.saved(option(list(Id.t))),
  };

  let mk = (~dynamics=Dynamics.empty, ~statics=CachedStatics.empty, editor) => {
    {
      editor,
      statics,
      dynamics,
      dynamic_statics: Calc.Pending,
      pinned_call: Calc.Pending,
    };
  };

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
        Option.bind(id, Dynamics.Map.lookup(_, model.dynamics.probe_map)),
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
        ~settings: CoreSettings.t,
        ~is_edited,
        ~ctx=?,
        ~stitch,
        ~dynamics: Calc.t(Dynamics.t),
        ~is_dynamic_term,
        {editor, statics, dynamic_statics, pinned_call, dynamics: _}: Model.t,
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

    let ctx_init: Ctx.t = Builtins.ctx_init(Some(Int));

    // Track the current pinned call state
    let current_pinned_call = Haz3lcore.ProbeProj.DynCursor.get_pinned_call();
    let pinned_call_t = Calc.set(current_pinned_call, pinned_call);

    let dynamic_statics =
      if (settings.dynamic_feedback) {
        Calc.Syntax.(
          dynamic_statics
          |> {
            let.calc dyn = dynamics
            and.calc pinned_call = pinned_call_t;

            let filtered_dynamics =
              Language.Dynamics.filter_all_by_pin(pinned_call, dyn);

            let dynamic_expressions: Id.Map.t(DynamicStatics.Map.entry) =
              Id.Map.map(
                List.map((c: Dynamics.Probe.Closure.t): DynamicStatics.sample =>
                  {exp: c.value}
                ),
                filtered_dynamics.probe_map,
              );

            let type_inst_probes: Id.Map.t(DynamicStatics.Map.type_inst_entry) =
              Id.Map.map(
                List.map(
                  (inst: Dynamics.TypeInstantiation.t): DynamicStatics.type_instantiation =>
                  {
                    tpat_id: inst.tpat_id,
                    type_var: inst.type_var,
                    instantiated_type: inst.instantiated_type,
                  }
                ),
                filtered_dynamics.type_inst_map,
              );

            let dynamic_info_map =
              Statics.mk(
                ~dynamics={
                  exp_probes: dynamic_expressions,
                  type_inst_probes,
                },
                settings,
                ctx_init,
                statics.term,
              );

            let dynamic_error_ids =
              StaticsBase.Map.error_ids(dynamic_info_map)
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
        ~is_edited,
        statics,
        Calc.get_value(dynamics).probe_map,
        editor,
      );
    {
      editor,
      statics,
      dynamics: Calc.get_value(dynamics),
      dynamic_statics: Calc.save(dynamic_statics),
      pinned_call: Calc.save(pinned_call_t),
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
