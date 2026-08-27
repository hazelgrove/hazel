open Util.WebUtil;
open Haz3lcore;

/* Read-only code viewer with statics, but no interaction. Notably,
   since there is no interaction, the user can see that there is an
   error but cannot select the error for more details. */

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  /* Context menu state lives in Util.Menu — None = closed, Some({…})
   * holds the selected item index and (unused for the editor menu) the
   * submenu path. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type context_menu_state = Util.Menu.t;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Updated:
    editor: Editor.t,
    context_menu: context_menu_state,
    statics: CachedStatics.t,
    dynamics: Language.Dynamics.Map.t,
  };

  let context_menu_is_open = (model: t): bool =>
    Util.Menu.is_open(model.context_menu);

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
    info: Indicated.ci_of(model.editor.state.zipper, model.statics.info_map),
    indicated_piece:
      Indicated.for_decoration(model.editor.state.zipper)
      |> Option.map(({piece, _}: Indicated.piece) => piece),
    selected_text:
      Some(
        () => {
          let z = model.editor.state.zipper;
          Printer.selected_text(
            ~indent=" ",
            ~refractors=z.refractors.manuals,
            z,
          );
        },
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

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = Editor.Model.persistent;
  let persist = (model: t) => model.editor |> Editor.Model.persist;
  let to_string = (model: t) => model.editor |> Editor.Model.to_string;
  let unpersist = p => p |> Editor.Model.unpersist |> mk;
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
        ~autoprobe_mode=false,
        ~is_edited,
        ~statics_mode=StaticsNormal,
        ~compositional=false,
        ~ctx=?,
        ~stitch,
        ~dynamics: Language.Dynamics.Map.t,
        ~is_dynamic_term,
        ~ana=?,
        {editor, statics, context_menu, _}: Model.t,
      )
      : Model.t => {
    /* Throttle gate: decide whether to do a full statics recompute this
     * frame. When we reuse, `statics` keeps its ref — CachedSyntax.calculate
     * then skips the shape pass via phys-eq on info_map/elaborated.
     * PROBE EXCEPTION: probe ids are an ANALYSIS input (per-node
     * probe_targets witnesses) — deferring the recompute lets this
     * frame's eval request go out with fresh targets but a stale map,
     * and the worker's incremental cache then replays sampleless until
     * the next edit. A probe change recomputes NOW (cheap: DefStatics
     * probe-aware dirtying re-analyzes only the probed item). */
    let probes_changed =
      Id.Map.compare(
        compare,
        CachedStatics.probe_ids_of_zipper(editor.state.zipper),
        statics.probe_ids,
      )
      != 0;
    let statics =
      statics_mode == StaticsForce
      || is_edited
      && statics_mode != StaticsDefer
      || probes_changed
        ? editor.root == Sort.Typ
            /* Typ-rooted cells: wrapped-alias statics (real InfoTyp
               entries for the inspector) under the provided ctx */
            ? CachedStatics.init_typ(~settings, ~ctx?, editor.state.zipper)
            : editor.root == Sort.Pat
                ? CachedStatics.init_pat(
                    ~settings,
                    ~ctx?,
                    editor.state.zipper,
                  )
                : editor.root == Sort.TPat
                    ? CachedStatics.init_tpat(
                        ~settings,
                        ~ctx?,
                        editor.state.zipper,
                      )
                    : compositional
                        /* whole-program editors: per-item statics (DefStatics) —
                           only the dirty items re-analyze, and no monolithic
                           whole-program recursion runs (browser stack overflow on
                           large programs) */
                        ? CachedStatics.init_compositional(
                            ~settings,
                            ~stitch,
                            ~root=editor.root,
                            editor.state.zipper,
                          )
                        : CachedStatics.init(
                            ~settings,
                            ~stitch,
                            ~ctx?,
                            ~ana?,
                            ~is_dynamic_term,
                            ~root=editor.root,
                            editor.state.zipper,
                          )
        : statics;

    let editor =
      Editor.Update.calculate(
        ~settings,
        ~autoprobe_mode,
        ~is_edited,
        statics,
        dynamics,
        editor,
      );

    /* Refresh `statics.targets` against the post-probe-effects refractors.
     * Cheap O(|probe_ids|) fold; only this field depends on refractors, so
     * the rest of statics stays valid. */
    let statics =
      CachedStatics.with_targets(~settings, editor.state.zipper, statics);
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

  /* Memo for the code text + error/warning arms — by far the most
     expensive vdom in the app (of_tile/shard walks over the whole
     program). None of it depends on DYNAMICS, yet every streamed
     result chunk re-renders the page and was rebuilding it (~1s per
     chunk on mega-2k). Keyed on the physical identities of every
     input (as Obj.t, compared with ===); identical nodes also
     short-circuit the virtual-dom diff by reference equality. LRU so
     a stack of cells + master all stay resident. */
  type memo_entry = {
    m_key: array(Obj.t),
    m_nodes: list(Node.t),
  };
  let view_memo: ref(list(memo_entry)) = ref([]);
  /* SMALL cap, and same-length entries evict each other: every key
     pins a whole GENERATION of segment/measured/info_map — on mega
     programs a deep LRU pinned hundreds of MB of superseded
     generations (heap death after a few edits). Same piece-count is
     a cheap same-editor-previous-generation proxy; a false hit just
     costs a recompute. */
  let view_memo_max = 4;
  let key_eq = (a: array(Obj.t), b: array(Obj.t)): bool => {
    let n = Array.length(a);
    Array.length(b) == n
    && {
      let rec go = i => i >= n || a[i] === b[i] && go(i + 1);
      go(0);
    };
  };

  let view =
      (~globals: Globals.t, ~overlays: list(Node.t)=[], model: Model.t) => {
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
    let buffer_ids = Selection.is_buffer(z.selection) ? selection_ids : [];
    let warning_ids =
      globals.settings.core.display_warnings ? model.statics.warning_ids : [];
    let key = [|
      Obj.repr(measured),
      Obj.repr(term_data),
      Obj.repr(shape_map),
      Obj.repr(segment),
      Obj.repr(info_map),
      Obj.repr(model.editor.syntax),
      Obj.repr(model.statics.error_ids),
      Obj.repr(warning_ids),
      Obj.repr(buffer_ids),
      Obj.repr(globals.font_metrics),
      Obj.repr(globals.settings),
    |];
    let nodes =
      switch (List.find_opt(e => key_eq(e.m_key, key), view_memo^)) {
      | Some(entry) =>
        /* refresh LRU position */
        view_memo := [entry, ...List.filter(e => !(e === entry), view_memo^)];
        entry.m_nodes;
      | None =>
        let refine_sort = (id, mold_out) =>
          Language.Info.refine_sort_from_mold(~info_map, ~id, mold_out);
        let code_text_view =
          CodeViewable.view(
            ~globals,
            ~measured,
            ~term_data,
            ~buffer_ids,
            ~shape_map,
            ~refractor_shape_map=Id.Map.empty, //Id.Map.map(_ => 2, z.refractors.map),
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
        let warning_decos =
          Arms.Errors.of_ids(
            ~refine_sort,
            ~is_warning=true,
            ~font_metrics=globals.font_metrics,
            ~syntax=model.editor.syntax,
            warning_ids,
          );
        // errors after warnings to prioritize errors over warnings
        let nodes = [code_text_view, warning_decos, error_decos];
        let rec take = (n, xs) =>
          switch (n, xs) {
          | (0, _)
          | (_, []) => []
          | (n, [x, ...xs]) => [x, ...take(n - 1, xs)]
          };
        let seg_len = List.length(segment);
        let same_len = (e: memo_entry) =>
          switch ((Obj.magic(e.m_key[3]): Segment.t)) {
          | s => List.length(s) == seg_len
          | exception _ => false
          };
        view_memo :=
          [
            {
              m_key: key,
              m_nodes: nodes,
            },
            ...take(
                 view_memo_max - 1,
                 List.filter(e => !same_len(e), view_memo^),
               ),
          ];
        nodes;
      };
    let container_classes =
      ["code-container"]
      @ (globals.meta_down ? ["meta-down"] : [])
      @ (globals.settings.show_row_lines ? ["show-row-lines"] : []);
    Node.div(~attrs=[Attr.classes(container_classes)], nodes @ overlays);
  };
};
