open Js_of_ocaml;
open Haz3lcore;
open Virtual_dom.Vdom;
type editor_id = string;
open Util;

/* A selectable editable code container component with statics and type-directed code completion. */
// This file follows conventions in [docs/ui-architecture.md]

module Model = CodeWithStatics.Model;

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Perform(Action.t)
    /* Perform, confined to the region described by the given target.
     * Emitted by sub-editor views (see SubEditor.re): the action is
     * vetted in `update` against the live zipper — rejected if it would
     * edit outside the splice, delete the host syntax at the splice's
     * edges, or move the caret out of the splice. View-level key guards
     * can't enforce this: they close over the render-time zipper, which
     * is stale during keystroke bursts. */
    | PerformConfined(SubEditor.Target.t, Action.t)
    | TAB
    | ContextMenu(ContextMenu.Model.action)
    | DebugConsole(string);

  exception CantReset;

  let can_undo = (action: t) => {
    switch (action) {
    | Perform(action)
    | PerformConfined(_, action) => Action.is_historic(action)
    | TAB => true
    | ContextMenu(_) => false
    | DebugConsole(_) => false
    };
  };

  let update =
      (~settings: Settings.t, action: t, model: Model.t): Updated.t(Model.t) => {
    let perform = (action: Action.t, model: Model.t) =>
      Editor.Update.update(
        ~settings=settings.core,
        action,
        model.statics,
        model.dynamics,
        model.editor,
      )
      |> (
        fun
        | Ok(editor) =>
          Model.{
            editor,
            statics: model.statics,
            dynamics: model.dynamics,
            context_menu: None,
          }
        | Error(err) => raise(Action.Failure.Exception(err))
      )
      |> Updated.return(
           ~is_edit=
             Action.is_edit(action)
             /* When probe_all is on, Refractor actions don't require
              * re-evaluation since all probes are already computed */
             && !(
                  settings.core.probe_all
                  && (
                    switch (action) {
                    | Probe(_) => true
                    | _ => false
                    }
                  )
                ),
           ~recalculate=true,
           ~scroll_active={
             switch (action) {
             | Move(Point(_)) => false
             | Select(All) => false
             | Select(Resize(Point(_))) => false
             | Move(_)
             | Select(_)
             | Destruct(_)
             | Insert(_)
             | Put_down
             | Buffer(Set(_) | Accept | Clear)
             | Paste(_)
             | Copy
             | Cut
             | Reparse
             | Introduce
             | PrettyPrint
             | Probe(StepInto(_))
             | Dump
             | ToggleLineComment => true
             | Project(_)
             | Unselect(_)
             | Structural(_)
             | Probe(_) => false
             };
           },
         );
    switch (action) {
    | Perform(action) =>
      settings.core.flip_animations && Action.should_animate(action)
        ? Animation.request([Animation.Actions.move("caret")]) : ();

      perform(action, model);
    | PerformConfined(target, action) =>
      switch (SubEditor.confine_pre(~target, ~action, model.editor)) {
      | None => model |> Updated.return_quiet
      | Some(sub) =>
        let updated = perform(action, model);
        Action.is_edit(action)
        || SubEditor.confine_post(sub, updated.model.editor.state.zipper)
          ? updated : model |> Updated.return_quiet;
      }
    | DebugConsole(key) =>
      DebugConsole.print(~settings, model, key);
      model |> Updated.return_quiet;
    | ContextMenu(action) =>
      let new_state =
        ContextMenu.WithContext.update(
          ~info_map=model.statics.info_map,
          ~elaborated=model.statics.elaborated,
          ~zipper=model.editor.state.zipper,
          action,
          model.context_menu,
        );
      {
        ...model,
        context_menu: new_state,
      }
      |> Updated.return_quiet;
    | TAB =>
      /* Attempt to act intelligently when TAB is pressed.
       * TODO: Consider more advanced TAB logic. Instead
       * of simply moving to next hole, if the backpack is non-empty
       * but can't immediately put down, move to next position of
       * interest, which is closet of: nearest position where can
       * put down, farthest position where can put down, next hole */
      let z = model.editor.state.zipper;
      let action: Action.t =
        Selection.is_buffer(z.selection)
          ? Buffer(Accept)
          : Zipper.can_put_down(z)
              ? Put_down : Move(Goal(NextProblem(Right)));
      perform(action, model);
    };
  };

  let calculate = CodeWithStatics.Update.calculate;
};

/* A capability handle on the main editor, passed down to views that
 * render sub-editors (see SubEditor.re): the current model to render
 * splices from, and an inject that routes editor actions back to the
 * main editor's update. Plumbed CellEditor → EvalResult → Theorems →
 * StepperView → step views alongside ~edit_syntax. */
module Channel = {
  type t = {
    model: Model.t,
    inject: Update.t => Ui_effect.t(unit),
  };
};

module Selection = {
  open Cursor;

  // Editor selection is handled within Editor.t
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = unit;

  let is_command_palette_open = (): bool => {
    let palette = JsUtil.get_elem_by_id("ninja-keys");
    Js.Unsafe.get(palette, "opened");
  };

  let get_cursor_info =
      (
        ~inject: Update.t => Ui_effect.t(unit),
        ~selection as (),
        model: Model.t,
      )
      : cursor(Update.t) => {
    let meta = Keyboard.meta();
    let mk = ContextualAction.mk;
    let action = a => inject(Perform(a));
    {
      ...
        CodeWithStatics.Model.get_cursor_info(model)
        |> map(x => Update.Perform(x)),
      editor_read_only: false,
    }
    |> Cursor.with_actions([
         /* Navigation */
         mk(
           ~hotkey="F12",
           ~mdIcon="arrow_forward",
           ~section="Navigation",
           ~action=action(Move(Goal(BindingSiteOfIndicatedVar))),
           "Go to Definition",
         ),
         mk(
           ~hotkey="shift+tab",
           ~mdIcon="arrow_upward",
           ~section="Navigation",
           ~action=action(Move(Goal(NextProblem(Left)))),
           "Go to Previous Problem",
         ),
         mk(
           ~mdIcon="arrow_downward",
           ~section="Navigation",
           ~action=action(Move(Goal(NextProblem(Right)))),
           "Go to Next Problem",
         ),
         /* Selection */
         mk(
           ~hotkey=meta ++ "+d",
           ~mdIcon="select_all",
           ~section="Selection",
           ~action=action(Select(Term(Current))),
           "Select current term",
         ),
         mk(
           ~mdIcon="select_all",
           ~hotkey=meta ++ "+a",
           ~section="Selection",
           ~action=action(Select(All)),
           "Select All",
         ),
         mk(
           ~mdIcon="flip_horizontal",
           ~section="Selection",
           ~action=action(Select(ToggleFocus)),
           "Toggle Selection Focus",
         ),
         mk(
           ~mdIcon="border_left",
           ~section="Selection",
           ~hotkey=meta ++ "+alt+shift+left",
           ~action=action(Select(SetFocus(Left))),
           "Set Selection Focus Left",
         ),
         mk(
           ~mdIcon="border_right",
           ~section="Selection",
           ~hotkey=meta ++ "+alt+shift+right",
           ~action=action(Select(SetFocus(Right))),
           "Set Selection Focus Right",
         ),
         mk(
           ~mdIcon="chevron_left",
           ~section="Selection",
           ~hotkey="alt+shift+left",
           ~action=action(Select(Resize(Local(Left, ByToken)))),
           "Extend Selection Left by Token",
         ),
         mk(
           ~mdIcon="chevron_right",
           ~section="Selection",
           ~hotkey="alt+shift+right",
           ~action=action(Select(Resize(Local(Right, ByToken)))),
           "Extend Selection Right by Token",
         ),
         /* Projection */
         mk(
           ~hotkey="alt+f",
           ~mdIcon="camera",
           ~section="Projection",
           ~action=action(Project(SetIndicated(Specific(Fold)))),
           "Fold",
         ),
         mk(
           ~hotkey=meta ++ "+e",
           ~mdIcon="camera",
           ~section="Projection",
           ~action=action(Probe(ToggleManual)),
           "Probe",
         ),
         mk(
           ~hotkey="alt+t",
           ~mdIcon="camera",
           ~section="Projection",
           ~action=action(Probe(ToggleStatics)),
           "Statics",
         ),
         mk(
           ~hotkey="alt+l",
           ~mdIcon="camera",
           ~section="Projection",
           ~action=action(Project(SetIndicated(ChooseLivelit))),
           "Livelit",
         ),
         /* Editor tools */
         mk(
           ~hotkey=meta ++ "+/",
           ~mdIcon="assistant",
           ~action=action(Buffer(Set(TyDi))),
           "TyDi Assistant",
         ),
         mk(
           ~section="Diagnostics",
           ~mdIcon="refresh",
           ~action=inject(Perform(Reparse)),
           "Reparse Current Editor",
         ),
         mk(
           ~mdIcon="bolt",
           ~section="Refactoring",
           ~hotkey=meta ++ "+i",
           ~action=action(Introduce),
           "Introduce",
         ),
         mk(
           ~mdIcon="format_align_left",
           ~section="Formatting",
           ~hotkey=meta ++ "+s",
           ~action=action(PrettyPrint),
           "Pretty Print",
         ),
       ]);
  };

  /* Focus the indicated probe (if any) */
  let focus_indicated_probe = (model: Model.t): option(Update.t) => {
    let z = model.editor.state.zipper;
    let refractors =
      z.refractors.manuals @ Id.Map.to_list(z.refractors.multis.ephemerals);
    switch (Indicated.index(z)) {
    | Some(id) =>
      switch (List.find_index(((rid, _)) => rid == id, refractors)) {
      | Some(idx) => Some(Update.Perform(Project(Focus(idx, Probe, None))))
      | None => None
      }
    | None => None
    };
  };

  /* Focus a probe on the current line (for end-of-line bounce) */
  let focus_probe_on_row = (model: Model.t): option(Update.t) => {
    let z = model.editor.state.zipper;
    let measured = CachedSyntax.measured(model.editor.syntax);
    let caret_row = Zipper.Caret.point(measured, z).row;
    let refractors =
      z.refractors.manuals @ Id.Map.to_list(z.refractors.multis.ephemerals);
    let probe_on_row =
      refractors
      |> List.find_index(((id, _)) =>
           switch (Measured.find_by_id(id, measured)) {
           | Some(m) => m.last.row == caret_row
           | None => false
           }
         );
    switch (probe_on_row) {
    | Some(idx) => Some(Update.Perform(Project(Focus(idx, Probe, None))))
    | None => None
    };
  };

  let handle_key_event =
      (~selection as (), model: Model.t): (Key.t => option(Update.t)) =>
    fun
    | {key: D("Escape"), _} when is_command_palette_open() =>
      /* Let Escape bubble so NinjaKeys can close itself. */
      None
    | {key: D("Tab"), sys: _, shift: Up, meta: Up, ctrl: Up, alt: Up, _} =>
      Some(Update.TAB)
    /* Cmd+Enter (Mac) / Ctrl+Enter (PC) focuses indicated probe */
    | {
        key: D("Enter"),
        sys: Mac,
        shift: Up,
        meta: Down,
        ctrl: Up,
        alt: Up,
        _,
      }
    | {key: D("Enter"), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up, _} =>
      switch (focus_indicated_probe(model)) {
      | Some(_) as result => result
      | None => focus_probe_on_row(model)
      }
    /* Cmd+Right (Mac) / End (PC) at end of line: bounce into probe */
    | {
        key: D("ArrowRight"),
        sys: Mac,
        shift: Up,
        meta: Down,
        ctrl: Up,
        alt: Up,
        _,
      }
        when
          Zipper.linebreak_on(
            Right,
            Zipper.generalized_neighbors(model.editor.state.zipper),
          ) =>
      switch (focus_probe_on_row(model)) {
      | Some(_) as result => result
      | None => Some(Update.Perform(Move(Line(Right))))
      }
    | {key: D("End"), sys: PC, shift: Up, meta: Up, ctrl: Up, alt: Up, _}
        when
          Zipper.linebreak_on(
            Right,
            Zipper.generalized_neighbors(model.editor.state.zipper),
          ) =>
      switch (focus_probe_on_row(model)) {
      | Some(_) as result => result
      | None => Some(Update.Perform(Move(Line(Right))))
      }
    /* Cmd+/ (Mac) / Ctrl+/ (PC) toggles line comment */
    | {key: D("/"), sys: Mac, shift: Up, meta: Down, ctrl: Up, alt: Up, _}
    | {key: D("/"), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up, _} =>
      Some(Update.Perform(ToggleLineComment))
    /* Cmd+. (Mac) / Ctrl+. (PC) opens context menu - VS Code Quick Fix convention */
    | {key: D("."), sys: Mac, shift: Up, meta: Down, ctrl: Up, alt: Up, _}
    | {key: D("."), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up, _} =>
      Some(Update.ContextMenu(ContextMenu.Model.Open))
    /* Shift+F10 opens context menu (VS Code convention) */
    | {key: D("F10"), sys: _, shift: Down, meta: Up, ctrl: Up, alt: Up, _} =>
      Some(Update.ContextMenu(ContextMenu.Model.Open))
    | {
        key: D(key),
        sys: Mac | PC,
        shift: Down,
        meta: Up,
        ctrl: Up,
        alt: Up,
        _,
      }
        when Keyboard.is_f_key(key) =>
      Some(Update.DebugConsole(key))
    | k =>
      Keyboard.handle_key_event(k) |> Option.map(x => Update.Perform(x));

  let handle_key_event = (~selection, model: Model.t, key: Key.t) =>
    /* Context menu key dispatch (Escape/ArrowUp/ArrowDown/Enter) is handled
     * at the document level by ContextMenuListener while the menu is open,
     * so it doesn't reach this handler. */
    switch (
      ProjectorView.key_handoff(
        model.editor,
        key,
        model.editor.syntax.projector_list,
      )
    ) {
    | Some(action) => Some(Update.Perform(Project(action)))
    | None => handle_key_event(~selection, model, key)
    };

  let jump_to_tile = (id: Id.t, model: Model.t): option(Update.t) => {
    switch (TermData.root_piece(id, model.editor.syntax.term_data)) {
    | Some(_) => Some(Perform(Move(Goal(TileId(id)))))
    | None => None
    };
  };
};

module View = {
  type event =
    | MakeActive;

  let container_target = (current_target: Js.opt(Js.t(Dom_html.element))) =>
    current_target
    |> Js.Opt.get(_, _ => failwith(""))
    |> JsUtil.get_child_with_class(_, "code-container")
    |> OptUtil.value_exn(
         ~none=Invalid_argument("CodeEditable.View.container_target"),
       );

  module PointerCapture = {
    /* This uses the Pointer Capture API to keep mouse movement data flowing
     * to an editor even when the mouse exits the editor element or even
     * browser window. This is necessary to (for example) be able to select
     * upwards while auto-scrolling the editor by flinging your mouse to the
     * top of your screen; otherwise, the selection action stops as the
     * mouse exits the editor element's bounding box. */

    let set = (target, pointer_id) =>
      JsUtil.setPointerCapture(container_target(target), pointer_id);

    let release = (target, pointer_id) =>
      if (JsUtil.hasPointerCapture(container_target(target), pointer_id)) {
        JsUtil.releasePointerCapture(container_target(target), pointer_id);
      };
  };

  module MouseState = Pointer.MkState();

  /* Toggle an `is-resizing` class on the editor's code-container
   * for the duration of a drag gesture. Used by CSS to suppress
   * caret-tracking decorations (e.g. variable highlights) while the
   * selection is being actively manipulated — including the
   * zero-width frame when a drag crosses back through the anchor. */
  module DragClass = {
    let name = Js.string("is-resizing");
    let add = (target: Js.opt(Js.t(Dom_html.element))): unit =>
      try(container_target(target)##.classList##add(name)) {
      | _ => ()
      };
    let remove = (target: Js.opt(Js.t(Dom_html.element))): unit =>
      try(container_target(target)##.classList##remove(name)) {
      | _ => ()
      };
  };

  let deco =
      (
        ~expand_selection=false,
        ~syntax: CachedSyntax.t,
        ~info_map: Language.Statics.Map.t,
        ~globals: Globals.t,
        z: Zipper.t,
      ) => [
    CaretDec.view(
      ~measured=CachedSyntax.measured(syntax),
      ~font_metrics=globals.font_metrics,
      z,
    ),
    Arms.Indicated.term(
      ~refine_sort=
        (id, mold_out) =>
          Language.Info.refine_sort_from_mold(~info_map, ~id, mold_out),
      ~font_metrics=globals.font_metrics,
      ~syntax,
      z,
    ),
    (
      expand_selection
        ? Highlight.selection_expanded(~term_data=syntax.term_data)
        : Highlight.selection
    )(
      ~measured=CachedSyntax.measured(syntax),
      ~shape_map=syntax.shape_map,
      ~font_metrics=globals.font_metrics,
      z,
    ),
    Backpack.view(
      ~font_metrics=globals.font_metrics,
      ~measured=CachedSyntax.measured(syntax),
      ~cached_backpack=syntax.cached_backpack,
      z,
    ),
    Highlight.colors(
      ~font_metrics=globals.font_metrics,
      ~syntax,
      globals.color_highlights,
    ),
    VarHighlight.view(
      ~measured=CachedSyntax.measured(syntax),
      ~font_metrics=globals.font_metrics,
      ~info_map,
      z,
    ),
  ];

  /* Recursive: a splice inside this editor renders as another instance
   * of this same view (see `render_splice` below). */
  let rec view =
          (
            ~globals: Globals.t,
            ~signal: event => Ui_effect.t(unit),
            ~edit_mode: EditMode.t(Update.t, unit),
            ~overlays: list(Node.t)=[],
            ~lines: bool=false,
            ~dynamics: Language.Dynamics.Map.t,
            ~predicted_reuse: option(Language.EvaluatorState.incr_eval)=?,
            ~pending_eval_ids: list(Id.t)=[],
            ~show_active_eval: bool=false,
            ~expand_selection=?,
            ~sub_editor: option(SubEditor.t)=None,
            model: Model.t,
          ) => {
    /* Sub-editor mode: render the main editor's model through a splice.
     * The displayed main_splice is overridden to be splice-local so
     * layout starts at a local origin, while the zipper, statics and
     * history remain the main editor's — the splice's pieces ARE main
     * editor pieces, so edits in either view are edits to the same
     * segment. */
    let is_sub = Option.is_some(sub_editor);
    /* Splice sub-editors route keys and pointer goals through the main
     * editor's own handlers (the zipper's splice context frames them);
     * region sub-editors are the focused surface and handle their own. */
    let is_splice_sub =
      switch (sub_editor) {
      | Some(sub) => SubEditor.is_splice_frame(sub)
      | None => false
      };
    let syntax =
      switch (sub_editor) {
      | Some(sub) => {
          ...model.editor.syntax,
          main_splice: sub.splice,
        }
      | None => model.editor.syntax
      };
    let model = {
      ...model,
      editor: {
        ...model.editor,
        syntax,
      },
    };

    let selected = EditMode.is_active(edit_mode);
    /* Caret ownership: the one zipper's caret belongs to exactly one
     * editor surface — the sub-editor whose region the caret is inside,
     * or the main editor when it's in no splice. Caret/selection decos
     * (whose measured lookups only make sense in the owning frame) are
     * gated on this. */
    let caret_here =
      switch (sub_editor) {
      | Some(sub) => SubEditor.owns_caret(sub, model.editor.state.zipper)
      | None => Zipper.splice_context(model.editor.state.zipper) == None
      };
    let inject_raw =
      switch (edit_mode) {
      | ReadOnly => (_ => Ui_effect.Ignore)
      | Editable({inject, _}) => inject
      };
    /* Sub-editor views emit ordinary Perform/TAB actions; rewrite them
     * here so every pointer/keyboard/clipboard path is confined — and
     * lands in the right coordinate frame — without callers remembering
     * to wrap. */
    let inject = (a: Update.t) =>
      switch (sub_editor, a) {
      | (Some(sub), Perform(action)) =>
        inject_raw(
          PerformConfined(sub.target, SubEditor.reframe_action(sub, action)),
        )
      | (Some(_), TAB) => Ui_effect.Ignore
      | (_, a) => inject_raw(a)
      };
    let escape =
      switch (edit_mode) {
      | ReadOnly => (_ => Ui_effect.Ignore)
      | Editable({escape, _}) => escape
      };
    /* Sync document-level listeners (click-outside + keyboard) for the
     * context menu. Keys are dispatched at capture phase so the editor's
     * window-level handler doesn't see them while the menu is open.
     * Sub-editors skip this: the listener registry is a singleton owned
     * by the main editor's view, and the context menu is disabled in
     * sub-editors anyway. */
    if (!is_sub) {
      ContextMenuListener.sync(
        ~menu_open=selected && Model.context_menu_is_open(model),
        ~on_close=inject(ContextMenu(ContextMenu.Model.Close)),
        ~handle_key=
          key_str =>
            ContextMenu.WithContext.handle_listener_key(
              ~info_map=model.statics.info_map,
              ~elaborated=model.statics.elaborated,
              ~syntax=model.editor.syntax,
              ~dynamics,
              ~zipper=model.editor.state.zipper,
              ~dispatch_menu=a => inject(ContextMenu(a)),
              ~dispatch_action=a => inject(Perform(a)),
              model.context_menu,
              key_str,
            ),
        (),
      );
    };
    let edit_decos =
      switch (sub_editor) {
      /* Lean decoration set for region sub-editors: caret + selection
       * only, and only while this frame owns the caret. Their measured
       * map covers the region alone, so the full set's lookups (which
       * range over the whole frame) would raise. Splice sub-editors are
       * complete frames and take the main path below. */
      | Some(sub) when !SubEditor.is_splice_frame(sub) =>
        selected && SubEditor.owns_caret(sub, model.editor.state.zipper)
          ? {
            let z = model.editor.state.zipper;
            let syntax = model.editor.syntax;
            switch (SubEditor.caret_point(sub, z)) {
            | None => []
            | Some(origin) =>
              /* Guard each deco separately: both can raise on measured
               * lookups of pieces just outside the splice (e.g. a
               * char-level selection anchored at the splice's first
               * piece), and a raising selection highlight must not
               * take the caret down with it. */
              let caret =
                switch (
                  CaretDec.view(
                    ~measured=CachedSyntax.measured(syntax),
                    ~font_metrics=globals.font_metrics,
                    ~origin,
                    z,
                  )
                ) {
                | exception _ => []
                | caret => [caret]
                };
              let selection =
                switch (
                  Highlight.selection(
                    ~measured=CachedSyntax.measured(syntax),
                    ~shape_map=syntax.shape_map,
                    ~font_metrics=globals.font_metrics,
                    z,
                  )
                ) {
                | exception _ => []
                | selection => [selection]
                };
              caret @ selection;
            };
          }
          : []
      | Some(_)
      | None =>
        selected && caret_here
          ? deco(
              ~expand_selection?,
              ~syntax=model.editor.syntax,
              ~info_map=model.statics.info_map,
              ~globals,
              model.editor.state.zipper,
            )
            @ [
              Arms.Refractors.all(
                ~font_metrics=globals.font_metrics,
                ~syntax=model.editor.syntax,
                ~dynamics,
                model.editor.state.zipper,
              ),
            ]
            @ (
              switch (model.context_menu) {
              | Some(_) => [
                  /* Backdrop for scroll-close. Click handling is done via
                     ContextMenuListener's document-level event listener. */
                  Node.div(
                    ~attrs=[
                      Attr.classes(["context-menu-backdrop"]),
                      Attr.on_wheel(_ =>
                        inject(ContextMenu(ContextMenu.Model.Close))
                      ),
                    ],
                    [],
                  ),
                  ContextMenu.view(
                    ~inject=a => inject(Perform(a)),
                    ~inject_menu=a => inject(ContextMenu(a)),
                    ~syntax=model.editor.syntax,
                    ~info_map=model.statics.info_map,
                    ~elaborated=model.statics.elaborated,
                    ~dynamics,
                    ~font_metrics=globals.font_metrics,
                    ~model=model.context_menu,
                    model.editor.state.zipper,
                  ),
                ]
              | None => []
              }
            )
          : []
      };
    // let t0 = JsUtil.precise_timestamp();
    let zipper = model.editor.state.zipper;
    /* Use visible row range from model (updated by scroll handler) */
    let visible = globals.visible_rows;
    /* Refractor / projector overlays are main-editor-level features whose
     * positions come from measured lookups over the whole buffer; the
     * splice-local measured map can't serve them, so sub-editors render
     * without them. */
    let refractors_model =
      if (is_sub) {
        [];
      } else {
        let refractor_data =
          RefractorView.mk_data(
            ~refractors=
              Id.Map.union(
                (_, _, b) => Some(b),
                zipper.refractors.manuals |> Id.Map.of_list,
                zipper.refractors.multis.ephemerals,
              ),
            ~syntax=model.editor.syntax,
            ~indicated=Indicated.for_decoration(zipper),
            ~statics=model.statics.info_map,
            ~dynamics,
            ~sample_focus=zipper.refractors.sample_focus,
            ~editor_active=selected,
          );
        RefractorView.all(
          x => inject(Perform(x)),
          signal(MakeActive),
          globals.font_metrics,
          ~core_settings=globals.settings.core,
          ~visible?,
          refractor_data,
          List.map(fst, zipper.refractors.manuals)
          @ List.map(
              fst,
              Id.Map.to_list(zipper.refractors.multis.ephemerals),
            ),
        );
      };
    // let t2 = JsUtil.precise_timestamp();
    /* Render a splice as a full recursive sub-editor over the same model:
     * the ~sub_editor argument swaps the splice's cached frame into
     * main_splice.
     *
     * A splice wrapper is just the simplest kind of sub-editor target —
     * `Target.of_splice` locates its whole content — so actions routed
     * from here go through the same confinement and reframing path as any
     * other sub-editor (see `inject` above and Update.PerformConfined). */
    let render_splice =
        (~projector_idx as _, ~splice_idx as _, splice: Base.splice): Node.t =>
      switch (
        SubEditor.mk(
          model.editor,
          ~target=SubEditor.Target.of_splice(splice.id),
        )
      ) {
      | None =>
        ProjectorView.default_render_splice(
          globals.font_metrics,
          ~projector_idx=0,
          ~splice_idx=0,
          splice,
        )
      | Some(sub) =>
        Node.div(
          ~attrs=[
            Attr.classes(["splice-editor", "inline-editor-wrapper"]),
            /* Contain pointer interactions: without this, splice clicks
             * bubble to the projector wrapper (stealing focus into the
             * projector) and drags reach the outer editor's handlers. */
            Attr.on_pointerdown(_ => Effect.Stop_propagation),
            Attr.on_mousemove(_ => Effect.Stop_propagation),
          ],
          [
            view(
              ~globals,
              ~signal,
              ~edit_mode,
              ~dynamics,
              ~predicted_reuse?,
              ~pending_eval_ids,
              ~show_active_eval,
              ~sub_editor=Some(sub),
              model,
            ),
          ],
        )
      };
    let projectors =
      ProjectorView.all(
        x => inject(Perform(x)),
        signal(MakeActive),
        globals.font_metrics,
        ~core_settings=globals.settings.core,
        ~visible=?is_sub ? None : visible,
        ~render_splice,
        ProjectorView.Model.mk(
          ~syntax=model.editor.syntax,
          /* Render this frame's projectors only; nested ones render in
           * their host splice's sub-editor. */
          ~projector_list=model.editor.syntax.main_splice.projector_list,
          ~indicated=Indicated.for_decoration(zipper),
          ~statics=model.statics.info_map,
          ~dynamics,
          ~sample_focus=zipper.refractors.sample_focus,
          ~editor_active=selected,
          ~elaborated=Some(model.statics.elaborated),
        ),
        /* Index list: action indices (Focus/SetModel/SetTerm/...) are
         * resolved by Perform against the global projector list, so
         * every frame must compute them against the same list. */
        model.editor.syntax.projector_list,
      );
    ProjectorView.ViewCache.log_frame();
    /* The nut-menu setting paints ReusePass predictions (frozen tint). Pending
     * evaluation highlights are transient progress feedback, so keep them on
     * while the worker is running. */
    let incr_eval_overlay =
      switch (
        predicted_reuse,
        globals.settings.show_incremental_deco || pending_eval_ids != [],
      ) {
      | (Some(predicted_reuse), true) => [
          Node.div(
            ~attrs=[Attr.classes(["code-deco", "incremental-deco"])],
            [
              Highlight.incr_eval(
                ~font_metrics=globals.font_metrics,
                ~syntax=model.editor.syntax,
                ~pending_eval_ids,
                ~show_active_eval,
                ~show_frozen=globals.settings.show_incremental_deco,
                predicted_reuse,
              ),
            ],
          ),
        ]
      | (None, _)
      | (Some(_), false) => []
      };
    let overlays =
      incr_eval_overlay
      @ [Node.div(~attrs=[Attr.classes(["code-deco"])], edit_decos)]
      @ [Node.div(~attrs=[Attr.classes(["overlays"])], overlays)]
      @ projectors
      @ refractors_model;
    let code_view = CodeWithStatics.View.view(~globals, ~overlays, model);

    let loc = (e: Pointer.Event.t) => {
      let raw =
        FontMetrics.get_goal(
          ~font_metrics=globals.font_metrics,
          container_target(e.current_target),
          e.loc,
        );
      /* Sub-editor pointer goals arrive relative to the displayed region.
       * For a region sub-editor that means translating into the host's
       * coordinates, since the zipper actions they feed resolve against
       * the main measured map; either way the goal is clamped to the
       * region so the caret can't be moused out of it. (A splice's own
       * frame starts at the origin, so this only clamps there, and
       * `reframe_action` sends the goal to that frame.) */
      switch (sub_editor) {
      | Some(sub) => SubEditor.translate_goal(sub, raw)
      | None => raw
      };
    };

    /* Pointer modifier → optional chunkiness override for
     * Select(Resize(Point(...))). Alt on Mac / Ctrl on PC swaps to the
     * non-default chunkiness (BySmart ↔ ByChar) per the "Character-level
     * mouse" setting. None means "use the settings default". */
    let drag_chunkiness_override =
        (pointer: Pointer.Event.t): option(Action.chunkiness) => {
      let modifier_held =
        switch (pointer.sys) {
        | Mac => pointer.alt == Down
        | PC => pointer.ctrl == Down
        };
      modifier_held
        ? Some(Keyboard.mouse_modifier_chunk(globals.settings.core)) : None;
    };

    let move_or_select = (mouse: Pointer.Event.t, pointer_id: int) =>
      switch (mouse) {
      | {button: Left, shift: Down, _} =>
        /* Shift+click extends (or starts) a selection and arms a
         * drag-resize. Registered without click-counting so a
         * following plain click starts a fresh streak. */
        MouseState.pointerdown_no_count(loc(mouse));
        PointerCapture.set(mouse.current_target, pointer_id);
        DragClass.add(mouse.current_target);
        Effect.Many([
          signal(MakeActive),
          inject(
            Perform(
              Select(
                Resize(Point(loc(mouse), drag_chunkiness_override(mouse))),
              ),
            ),
          ),
        ]);
      | {button: Left, sys: PC, ctrl: Down, _}
      | {button: Left, sys: Mac, meta: Down, _} =>
        Effect.Many([
          signal(MakeActive),
          inject(Perform(Move(Point(loc(mouse), None)))),
          inject(Perform(Move(Goal(BindingSiteOfIndicatedVar)))),
        ])
      | {button: Right, ctrl, _} when ctrl != Down =>
        Effect.Many([
          //Effect.Stop_propagation,
          Effect.Prevent_default,
          inject(Perform(Move(Point(loc(mouse), None)))),
          inject(ContextMenu(ContextMenu.Model.Toggle)),
        ])
      | {button: Left, _} =>
        MouseState.pointerdown(loc(mouse));
        DragClass.add(mouse.current_target);
        let click_count = MouseState.count();
        /* Check how many clicks have happened recently
         * and cycle between options on-click */
        switch (click_count mod 3 + 1) {
        | 1 =>
          /* prepare to drag if the mouse moves */
          PointerCapture.set(mouse.current_target, pointer_id);
          Effect.Many([
            signal(MakeActive),
            inject(Perform(Move(Point(loc(mouse), None)))),
          ]);
        | 2 => inject(Perform(Select(Smart(2))))
        /* Triple-click's line-select would grab the whole main-editor
         * line the splice sits on; cap it at term-select in sub-editors. */
        | 3 => inject(Perform(Select(Smart(is_sub ? 2 : 3))))
        | _ => failwith("THEN PERISH")
        };
      | _ => Effect.Ignore
      };

    let toggle_button = (e: Pointer.Event.t, pointer_id: int) => {
      MouseState.pointerup(loc(e));
      PointerCapture.release(e.current_target, pointer_id);
      DragClass.remove(e.current_target);
      EdgeScroll.stop();
      Effect.Ignore;
    };

    let drag_select_or_hover = (pointer: Pointer.Event.t) => {
      let left_button_held = pointer.buttons land 1 != 0;
      if (!left_button_held && MouseState.is_button_down()) {
        /* Recover from stuck state: buttons bitmask says left is up
         * but MouseState thinks it's down (missed pointerup) */
        MouseState.reset();
        DragClass.remove(pointer.current_target);
        EdgeScroll.stop();
        Effect.Ignore;
      } else {
        let current_loc = loc(pointer);
        if (left_button_held && MouseState.is_button_down()) {
          MouseState.note_move(current_loc);
        };
        /* Suppress Resize while the cursor has never left the
         * pointerdown column (avoids spurious post-click scroll).
         * Once the cursor has departed, even mousemoves that return
         * to the down-loc dispatch Resize — this is required so the
         * selection can pass through zero-width when dragging back
         * across the anchor. */
        let at_down_loc_without_motion =
          !MouseState.has_left_down_loc()
          && Point.equals(current_loc, MouseState.get_down_loc());
        switch (pointer) {
        | {button: Left, _}
            when
              left_button_held
              && MouseState.is_button_down()
              && !at_down_loc_without_motion =>
          let container = container_target(pointer.current_target);
          let pixel_loc = pointer.loc;
          /* Snapshot at mousemove time so edge-scroll fires with the
           * same chunkiness mode the user had selected. */
          let chunk_override = drag_chunkiness_override(pointer);
          /* No edge-scroll for sub-editors: they're inline single-line
           * views, and the scroll callback's goal lookup bypasses the
           * splice coordinate translation. */
          if (!is_sub) {
            EdgeScroll.update(
              ~client_y=float_of_int(pointer.loc.row),
              ~on_scroll=() => {
                let goal =
                  FontMetrics.get_goal(
                    ~font_metrics=globals.font_metrics,
                    container,
                    pixel_loc,
                  );
                Bonsai.Effect.Expert.handle(
                  inject(
                    Perform(Select(Resize(Point(goal, chunk_override)))),
                  ),
                );
              },
            );
          };
          inject(
            Perform(Select(Resize(Point(current_loc, chunk_override)))),
          );
        | _ => Effect.Ignore
        };
      };
    };

    let display_line_numbers: bool = lines && globals.settings.line_numbers;

    let key_handler_attr =
      if (!selected) {
        /* Always focusable so first click gives DOM focus.
         * Key events are ignored when not selected — they bubble
         * to Page.re which handles page-level shortcuts. */
        Attr.tabindex(
          0,
        );
      } else {
        let z = model.editor.state.zipper;
        /* Editor-level clipboard helpers. Bypass the page-level
           on_copy/on_paste path because Firefox refuses to dispatch
           native clipboard events to non-editable focused elements
           (the editor div has tabindex(0) but is not contenteditable). */
        let selection_has_refractors =
            (refractors: Haz3lcore.Zipper.Refractor.t, selection) =>
          if (List.is_empty(refractors.manuals)) {
            false;
          } else {
            let ids = Haz3lcore.Segment.ids(selection);
            List.exists(
              id =>
                List.exists(
                  ((id2, _)) => Id.equal(id, id2),
                  refractors.manuals,
                ),
              ids,
            );
          };
        let copy_selection = () => {
          let segment = z.selection.content;
          let full =
            Printer.of_segment(
              ~indent=" ",
              ~refractors=z.refractors.manuals,
              segment,
            );
          let str = Zipper.trim_selected_text(z, full);
          /* Cache for paste reuse only when nothing was trimmed: a trimmed
             sub-token string must re-parse on paste, not round-trip to the
             full segment. */
          if (str == full && !selection_has_refractors(z.refractors, segment)) {
            Haz3lcore.Parser.set_segment_cache(Some(segment), str);
          };
          JsUtil.write_clipboard(str);
        };
        let paste_from_clipboard = () =>
          JsUtil.read_clipboard(text => {
            let action =
              Haz3lcore.Action.Paste(Util.StringUtil.trim_leading(text));
            Bonsai.Effect.Expert.handle(inject(Perform(action)));
          });
        /* Editing-boundary predicates. For the main editor the boundary
         * is the buffer's extremes; for a sub-editor it's the splice's
         * edges (the caret must not move into, or delete, the host
         * syntax surrounding the splice). */
        let is_at_left = () =>
          switch (sub_editor) {
          | Some(sub) => SubEditor.caret_at_left(sub, z)
          | None =>
            z.caret == Outer
            && z.relatives.ancestors == []
            && fst(Siblings.neighbors(z.relatives.siblings)) == None
          };
        let is_at_right = () =>
          switch (sub_editor) {
          | Some(sub) => SubEditor.caret_at_right(sub, z)
          | None =>
            z.caret == Outer
            && z.relatives.ancestors == []
            && snd(Siblings.neighbors(z.relatives.siblings)) == None
          };
        Key.handler(~f=key => {
          /* 1. Check for arrow key escape at boundaries FIRST.
           *    Keyboard.handle_key_event always returns Some for arrows,
           *    so boundary escape must be checked before delegation.
           *    Note: for sub-editors these view-level guards are UX
           *    only — `z` is the render-time zipper, stale during
           *    keystroke bursts. Enforcement happens at update time
           *    (Update.PerformConfined). */
          switch (key) {
          | {
              key: D("ArrowLeft" | "ArrowUp"),
              shift: Up,
              meta: Up,
              ctrl: Up,
              alt: Up,
              _,
            }
              when is_at_left() =>
            Effect.Many([Effect.Prevent_default, escape(Left)])
          | {
              key: D("ArrowRight" | "ArrowDown"),
              shift: Up,
              meta: Up,
              ctrl: Up,
              alt: Up,
              _,
            }
              when is_at_right() =>
            Effect.Many([Effect.Prevent_default, escape(Right)])
          /* Sub-editor caret confinement (UX layer; see note above):
           * keys whose main-editor semantics would take the caret out
           * of the splice (vertical and line-wise moves, whole-buffer
           * select) are swallowed. */
          | {
              key:
                D(
                  "ArrowUp" | "ArrowDown" | "Home" | "End" | "PageUp" |
                  "PageDown",
                ),
              _,
            }
              when is_sub => Effect.Prevent_default
          | {key: D("a" | "A"), sys: Mac, meta: Down, ctrl: Up, alt: Up, _}
          | {key: D("a" | "A"), sys: PC, meta: Up, ctrl: Down, alt: Up, _}
              when is_sub => Effect.Prevent_default
          /* 2. Cmd/Ctrl + C/X/V handled here rather than via the page
             on_copy/on_paste handlers, so they keep working in
             Firefox when focus is on a non-editable editor div. */
          | {
              key: D("c" | "C"),
              sys: Mac,
              shift: Up,
              meta: Down,
              ctrl: Up,
              alt: Up,
              _,
            }
          | {
              key: D("c" | "C"),
              sys: PC,
              shift: Up,
              meta: Up,
              ctrl: Down,
              alt: Up,
              _,
            } =>
            copy_selection();
            Effect.Many([Effect.Prevent_default, Effect.Stop_propagation]);
          | {
              key: D("x" | "X"),
              sys: Mac,
              shift: Up,
              meta: Down,
              ctrl: Up,
              alt: Up,
              _,
            }
          | {
              key: D("x" | "X"),
              sys: PC,
              shift: Up,
              meta: Up,
              ctrl: Down,
              alt: Up,
              _,
            } =>
            copy_selection();
            Effect.Many([
              Effect.Prevent_default,
              Effect.Stop_propagation,
              inject(Perform(Destruct(Right))),
            ]);
          | {
              key: D("v" | "V"),
              sys: Mac,
              shift: Up,
              meta: Down,
              ctrl: Up,
              alt: Up,
              _,
            }
          | {
              key: D("v" | "V"),
              sys: PC,
              shift: Up,
              meta: Up,
              ctrl: Down,
              alt: Up,
              _,
            } =>
            paste_from_clipboard();
            Effect.Many([Effect.Prevent_default, Effect.Stop_propagation]);
          | _ =>
            /* 3. Normal editor key handling:
             *    context menu → projector handoff → Keyboard */
            switch (Selection.handle_key_event(~selection=(), model, key)) {
            | Some(action) =>
              Effect.Many([
                Effect.Prevent_default,
                Effect.Stop_propagation,
                inject(action),
              ])
            | None => Effect.Ignore
            }
          }
        });
      };
    Node.div(
      ~attrs=[
        Attr.classes(
          ["cell-item", "code-editor"]
          @ (selected ? ["selected"] : [])
          @ (is_sub ? ["sub-editor"] : [])
          @ (display_line_numbers ? ["has-line-numbers"] : []),
        ),
        /* Tag the active cell so a sidebar jump can move DOM focus to it
           (see JsUtil.active_cell_id / ProbePerform.FocusEffect). */
        selected && !is_splice_sub
          ? Attr.id(JsUtil.active_cell_id) : Attr.empty,
        /* Splice sub-editors don't handle keys themselves: key events
         * route through the main editor (or page) and act on the one
         * zipper, whose splice context frames them correctly. A key
         * handler here would double-dispatch. */
        is_splice_sub ? Attr.empty : key_handler_attr,
        Attr.on_contextmenu(evt =>
          switch (Pointer.Event.mk(evt)) {
          | {button: Right, ctrl: Up, _} =>
            Effect.Many([Effect.Stop_propagation, Effect.Prevent_default])
          | _ => Effect.Ignore
          }
        ),
        Attr.on_pointerdown(evt =>
          move_or_select(Pointer.Event.mk(evt), Pointer.Event.id_of(evt))
        ),
        Attr.on_pointerup(evt =>
          toggle_button(Pointer.Event.mk(evt), Pointer.Event.id_of(evt))
        ),
        Attr.on_mousemove(evt =>
          drag_select_or_hover(Pointer.Event.mk(evt))
        ),
      ],
      display_line_numbers
        ? LineNumbers.View.view(
            model,
            globals.settings.relative_line_numbers,
            selected,
          )
          @ [code_view]
        : [code_view],
    );
  };
};
