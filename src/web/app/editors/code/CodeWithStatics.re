open Util;
open Haz3lcore;
open Language;
open WebUtil;

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
    dynamics: Dynamics.t,
    dynamic_statics: Calc.saved((StaticsBase.Map.t, list(Id.t))),
    sample_cursor: Calc.saved(Language.Sample.Cursor.t),
  };

  let context_menu_is_open = (model: t): bool => model.context_menu != None;

  let mk = (~dynamics=Dynamics.empty, ~statics=CachedStatics.empty, editor) => {
    {
      editor,
      statics,
      dynamics,
      context_menu: None,
      dynamic_statics: Calc.Pending,
      sample_cursor: Calc.Pending,
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
        ~ana=?,
        {
          editor,
          statics,
          dynamic_statics,
          sample_cursor,
          context_menu,
          dynamics: _,
        }: Model.t,
      )
      : Model.t => {
    let dynamics_map = Calc.map(dynamics, (d: Dynamics.t) => d.probe_map);
    let editor =
      Editor.Update.calculate(
        ~settings,
        ~is_edited,
        statics,
        dynamics_map |> Calc.get_value,
        editor,
      );
    let statics =
      is_edited
        ? CachedStatics.init(
            ~settings,
            ~stitch,
            ~ctx?,
            ~ana?,
            ~is_dynamic_term,
            editor.state.zipper,
          )
        : statics;

    let ctx_init: Ctx.t = Builtins.ctx_init(Some(Int));

    // Track the current sample cursor state
    let current_sample_cursor = editor.state.zipper.refractors.sample_cursor;
    let sample_cursor_calc =
      Calc.set(~eq=Sample.Cursor.equal, current_sample_cursor, sample_cursor);

    let dynamic_statics =
      if (settings.live_typing) {
        Calc.Syntax.(
          dynamic_statics
          |> {
            let.calc dyn = dynamics
            and.calc curr_sample_cursor = sample_cursor_calc;

            let filtered_dynamics =
              Language.Dynamics.filter_by_cursor(curr_sample_cursor, dyn);

            let dynamic_expressions: Id.Map.t(DynamicStatics.Map.entry) =
              Id.Map.map(
                List.map((sample: Sample.t): DynamicStatics.sample =>
                  {exp: sample.value}
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
        dynamics_map |> Calc.get_value,
        editor,
      );
    {
      editor,
      statics,
      dynamics: Calc.get_value(dynamics),
      dynamic_statics: Calc.save(dynamic_statics),
      sample_cursor: Calc.save(sample_cursor_calc),
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
        ~refractor_shape_map=Id.Map.empty,
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
    let container_classes =
      ["code-container"] @ (globals.meta_down ? ["meta-down"] : []);
    Node.div(
      ~attrs=[Attr.classes(container_classes)],
      [code_text_view, statics_decos, dynamic_static_decos] @ overlays,
    );
  };
};
