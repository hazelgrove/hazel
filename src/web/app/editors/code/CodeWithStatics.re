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
    live_typing: Calc.saved((StaticsBase.Map.t, list(Id.t))),
    sample_focus: Calc.saved(Language.Sample.Focus.t),
  };

  let context_menu_is_open = (model: t): bool => model.context_menu != None;

  let mk = (~dynamics=Dynamics.empty, ~statics=CachedStatics.empty, editor) => {
    {
      editor,
      statics,
      dynamics,
      context_menu: None,
      live_typing: Calc.Pending,
      sample_focus: Calc.Pending,
    };
  };

  let mk_from_exp =
      (
        ~settings: Language.CoreSettings.t,
        ~inline=false,
        ~root: Sort.t,
        term: Language.Exp.t,
      ) => {
    let seg =
      ExpToSegment.exp_to_segment(
        term,
        ~settings=ExpToSegment.Settings.of_core(~inline, settings),
      );
    let seg = inline ? seg : PrettySegment.prettify(seg);
    seg |> Zipper.unzip |> Editor.Model.mk(~root) |> mk;
  };

  let get_statics = (model: t) => model.statics;

  let get_cursor_info = (model: t): Cursor.cursor(Action.t) => {
    let info =
      Indicated.ci_of(model.editor.state.zipper, model.statics.info_map);
    let live_typing_info =
      Indicated.ci_of(
        model.editor.state.zipper,
        model.statics.live_typing_info_map,
      );
    let id = Indicated.index(model.editor.state.zipper);
    {
      info,
      live_typing_info,
      dynamics:
        Option.bind(id, Dynamics.Map.lookup(_, model.dynamics.probe_map)),
      indicated_piece:
        Indicated.for_decoration(model.editor.state.zipper)
        |> Option.map(({piece, _}: Indicated.piece) => piece),
      selected_text:
        Some(
          () =>
            Printer.of_segment(
              ~indent=" ",
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
      contextual_actions: [],
    };
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = Editor.Model.persistent;
  let persist = (model: t) => model.editor |> Editor.Model.persist;
  let to_string = (model: t) => model.editor |> Editor.Model.to_string;
  let unpersist = p => p |> Editor.Model.unpersist |> mk;
  let sort = (model: t): Sort.t => model.editor.root;
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
        ~settings: CoreSettings.t,
        ~autoprobe_mode=false,
        ~is_edited,
        ~statics_mode=StaticsNormal,
        ~ctx=?,
        ~stitch,
        ~dynamics: Calc.t(Dynamics.t),
        ~is_dynamic_term,
        ~ana=?,
        {
          editor,
          statics,
          live_typing,
          sample_focus,
          context_menu,
          dynamics: _,
        }: Model.t,
      )
      : Model.t => {
    let dynamics_map = Calc.map(dynamics, (d: Dynamics.t) => d.probe_map);
    /* Capture ephemerals before editor calculation to detect auto probe changes */
    let old_ephemerals = editor.state.zipper.refractors.multis.ephemerals;

    let editor =
      Editor.Update.calculate(
        ~settings,
        ~autoprobe_mode,
        ~is_edited,
        statics,
        dynamics_map |> Calc.get_value,
        editor,
      );

    /* Ephemerals can change without an explicit edit in several cases:
     * (1) cursor movement in autoprobe mode (cursor crosses into a new
     *     top-level definition), and
     * (2) on reload, when add_ids_from_multi_term rebuilds ephemerals
     *     from persisted multis.ids once the info_map becomes available.
     * In both cases we must recalculate statics so probe targets match
     * the new ephemerals and the evaluator collects samples for them. */
    let probes_changed =
      !
        Id.Map.equal(
          Refractors.equal_entry,
          old_ephemerals,
          editor.state.zipper.refractors.multis.ephemerals,
        );

    let statics =
      statics_mode == StaticsForce
      || (is_edited || probes_changed)
      && statics_mode != StaticsDefer
        ? CachedStatics.init(
            ~settings,
            ~stitch,
            ~ctx?,
            ~ana?,
            ~is_dynamic_term,
            ~root=editor.root,
            editor.state.zipper,
          )
        : statics;

    let ctx_init: Ctx.t = Builtins.ctx_init(Some(Int));

    // Track the current sample focus state
    let current_sample_focus = editor.state.zipper.refractors.sample_focus;
    let sample_focus_calc =
      Calc.set(~eq=Sample.Focus.equal, current_sample_focus, sample_focus);

    let live_typing =
      if (settings.live_typing) {
        Calc.Syntax.(
          live_typing
          |> {
            let.calc dyn = dynamics
            and.calc curr_sample_focus = sample_focus_calc;

            let filtered_dynamics =
              Language.Dynamics.filter_by_focus(curr_sample_focus, dyn);

            let dynamic_expressions: Id.Map.t(LiveTyping.Map.entry) =
              Id.Map.map(
                List.map((sample: Sample.t): LiveTyping.sample =>
                  {exp: sample.value}
                ),
                filtered_dynamics.probe_map,
              );

            let type_inst_probes: Id.Map.t(LiveTyping.Map.type_inst_entry) =
              Id.Map.map(
                List.map(
                  (inst: Dynamics.TypeInstantiation.t): LiveTyping.type_instantiation =>
                  {
                    tpat_id: inst.tpat_id,
                    type_var: inst.type_var,
                    instantiated_type: inst.instantiated_type,
                  }
                ),
                filtered_dynamics.type_inst_map,
              );

            let (live_typing_info_map, _) =
              Statics.mk(
                ~dynamics={
                  exp_probes: dynamic_expressions,
                  type_inst_probes,
                },
                settings,
                ctx_init,
                statics.term,
              );

            let live_typing_error_ids =
              StaticsBase.Map.error_ids(live_typing_info_map)
              |> List.filter(id => !List.mem(id, statics.error_ids));

            (live_typing_info_map, live_typing_error_ids);
          }
        );
      } else {
        Calc.set((StaticsBase.Map.empty, []), live_typing);
      };

    let statics: CachedStatics.t = {
      ...statics,
      live_typing_info_map: live_typing |> Calc.get_value |> fst,
      live_typing_error_ids: live_typing |> Calc.get_value |> snd,
    };

    let editor =
      Editor.Update.calculate(
        ~settings,
        ~autoprobe_mode,
        ~is_edited,
        statics,
        dynamics_map |> Calc.get_value,
        editor,
      );
    {
      editor,
      statics,
      dynamics: Calc.get_value(dynamics),
      live_typing: Calc.save(live_typing),
      sample_focus: Calc.save(sample_focus_calc),
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
    let info_map = model.statics.info_map;
    let refine_sort = (id, mold_out) =>
      Language.Info.refine_sort_from_mold(~info_map, ~id, mold_out);
    let code_text_view =
      CodeViewable.view(
        ~globals,
        ~measured,
        ~term_data,
        ~buffer_ids=Selection.is_buffer(z.selection) ? selection_ids : [],
        ~shape_map,
        ~refractor_shape_map=Id.Map.empty,
        ~refine_sort,
        segment,
      );
    let error_decos =
      Arms.Errors.of_ids(
        ~refine_sort,
        ~font_metrics=globals.font_metrics,
        ~syntax=model.editor.syntax,
        model.statics.error_ids,
      );
    let warning_ids =
      globals.settings.core.display_warnings ? model.statics.warning_ids : [];
    let warning_decos =
      Arms.Errors.of_ids(
        ~kind=Warning,
        ~refine_sort,
        ~font_metrics=globals.font_metrics,
        ~syntax=model.editor.syntax,
        warning_ids,
      );
    let live_typing_decos =
      Arms.Errors.of_ids(
        ~kind=LiveTypingError,
        ~refine_sort,
        ~font_metrics=globals.font_metrics,
        ~syntax=model.editor.syntax,
        model.statics.live_typing_error_ids,
      );
    let container_classes =
      ["code-container"]
      @ (globals.meta_down ? ["meta-down"] : [])
      @ (globals.settings.show_row_lines ? ["show-row-lines"] : []);
    Node.div(
      ~attrs=[Attr.classes(container_classes)],
      // errors after warnings to prioritize errors over warnings
      [code_text_view, warning_decos, error_decos, live_typing_decos]
      @ overlays,
    );
  };
};
