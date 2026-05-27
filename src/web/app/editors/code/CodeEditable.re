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
    | TAB
    | ContextMenu(ContextMenu.Model.action)
    | DebugConsole(string);

  exception CantReset;

  let can_undo = (action: t) => {
    switch (action) {
    | Perform(action) => Action.is_historic(action)
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
           ~hotkey="alt+p",
           ~mdIcon="play_circle",
           ~section="Projection",
           ~action=action(Probe(TogglePlayer)),
           "Player",
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
    let measured = model.editor.syntax.measured;
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
    switch (TermData.root_tile(id, model.editor.syntax.term_data)) {
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

  let deco =
      (
        ~expand_selection=false,
        ~syntax: CachedSyntax.t,
        ~info_map: Language.Statics.Map.t,
        ~globals: Globals.t,
        z: Zipper.t,
      ) => [
    CaretDec.view(
      ~measured=syntax.measured,
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
      ~measured=syntax.measured,
      ~shape_map=syntax.shape_map,
      ~font_metrics=globals.font_metrics,
      z,
    ),
    Backpack.view(
      ~font_metrics=globals.font_metrics,
      ~measured=syntax.measured,
      ~cached_backpack=syntax.cached_backpack,
      z,
    ),
    Highlight.colors(
      ~font_metrics=globals.font_metrics,
      ~syntax,
      globals.color_highlights,
    ),
    VarHighlight.view(
      ~measured=syntax.measured,
      ~font_metrics=globals.font_metrics,
      ~info_map,
      z,
    ),
  ];

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~edit_mode: EditMode.t(Update.t, unit),
        ~overlays: list(Node.t)=[],
        ~lines: bool=false,
        ~dynamics: Language.Dynamics.Map.t,
        ~incr_eval: Language.IncrEval.t=Language.IncrEval.empty,
        ~expand_selection=?,
        model: Model.t,
      ) => {
    let selected = EditMode.is_active(edit_mode);
    let inject =
      switch (edit_mode) {
      | ReadOnly => (_ => Ui_effect.Ignore)
      | Editable({inject, _}) => inject
      };
    let escape =
      switch (edit_mode) {
      | ReadOnly => (_ => Ui_effect.Ignore)
      | Editable({escape, _}) => escape
      };
    /* Sync document-level listeners (click-outside + keyboard) for the
     * context menu. Keys are dispatched at capture phase so the editor's
     * window-level handler doesn't see them while the menu is open. */
    ContextMenuListener.sync(
      ~menu_open=selected && Model.context_menu_is_open(model),
      ~on_close=inject(ContextMenu(ContextMenu.Model.Close)),
      ~handle_key=
        key_str =>
          ContextMenu.WithContext.handle_listener_key(
            ~info_map=model.statics.info_map,
            ~elaborated=model.statics.elaborated,
            ~zipper=model.editor.state.zipper,
            ~dispatch_menu=a => inject(ContextMenu(a)),
            ~dispatch_action=a => inject(Perform(a)),
            model.context_menu,
            key_str,
          ),
      (),
    );
    let edit_decos =
      selected
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
                  ~font_metrics=globals.font_metrics,
                  ~model=model.context_menu,
                  model.editor.state.zipper,
                ),
              ]
            | None => []
            }
          )
        : [];
    // let t0 = JsUtil.precise_timestamp();
    let zipper = model.editor.state.zipper;
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
    // let t1 = JsUtil.precise_timestamp();
    /* Use visible row range from model (updated by scroll handler) */
    let visible = globals.visible_rows;
    let refractors_model =
      RefractorView.all(
        x => inject(Perform(x)),
        signal(MakeActive),
        globals.font_metrics,
        ~core_settings=globals.settings.core,
        ~visible?,
        refractor_data,
        List.map(fst, zipper.refractors.manuals)
        @ List.map(fst, Id.Map.to_list(zipper.refractors.multis.ephemerals)),
      );
    // let t2 = JsUtil.precise_timestamp();
    let projectors =
      ProjectorView.all(
        x => inject(Perform(x)),
        signal(MakeActive),
        globals.font_metrics,
        ~core_settings=globals.settings.core,
        ~visible?,
        ProjectorView.Model.mk(
          ~syntax=model.editor.syntax,
          ~indicated=Indicated.for_decoration(zipper),
          ~statics=model.statics.info_map,
          ~dynamics,
          ~sample_focus=zipper.refractors.sample_focus,
          ~editor_active=selected,
          ~elaborated=Some(model.statics.elaborated),
        ),
        model.editor.syntax.projector_list,
      );
    ProjectorView.ViewCache.log_frame();
    /* Tint the background behind ids reused from the last run (cache hits)
     * with an icy wash, so the user can see what the incremental evaluator
     * is skipping. Gated behind a nut-menu setting because it's distracting
     * during normal editing. */
    let incr_eval_overlay =
      if (globals.settings.show_incremental_deco) {
        [
          Node.div(
            ~attrs=[Attr.classes(["code-deco", "incremental-deco"])],
            [
              Highlight.incr_eval(
                ~font_metrics=globals.font_metrics,
                ~syntax=model.editor.syntax,
                incr_eval,
              ),
            ],
          ),
        ];
      } else {
        [];
      };
    let overlays =
      incr_eval_overlay
      @ [Node.div(~attrs=[Attr.classes(["code-deco"])], edit_decos)]
      @ [Node.div(~attrs=[Attr.classes(["overlays"])], overlays)]
      @ projectors
      @ refractors_model;
    let code_view = CodeWithStatics.View.view(~globals, ~overlays, model);

    let loc = (e: Pointer.Event.t) =>
      FontMetrics.get_goal(
        ~font_metrics=globals.font_metrics,
        container_target(e.current_target),
        e.loc,
      );

    let move_or_select = (mouse: Pointer.Event.t, pointer_id: int) =>
      switch (mouse) {
      | {button: Left, shift: Down, _} =>
        Effect.Many([
          signal(MakeActive),
          inject(Perform(Select(Resize(Point(loc(mouse)))))),
        ])
      | {button: Left, sys: PC, ctrl: Down, _}
      | {button: Left, sys: Mac, meta: Down, _} =>
        Effect.Many([
          signal(MakeActive),
          inject(Perform(Move(Point(loc(mouse))))),
          inject(Perform(Move(Goal(BindingSiteOfIndicatedVar)))),
        ])
      | {button: Right, ctrl, _} when ctrl != Down =>
        Effect.Many([
          //Effect.Stop_propagation,
          Effect.Prevent_default,
          inject(Perform(Move(Point(loc(mouse))))),
          inject(ContextMenu(ContextMenu.Model.Toggle)),
        ])
      | {button: Left, _} =>
        MouseState.pointerdown(loc(mouse));
        let click_count = MouseState.count();
        /* Check how many clicks have happened recently
         * and cycle between options on-click */
        switch (click_count mod 3 + 1) {
        | 1 =>
          /* prepare to drag if the mouse moves */
          PointerCapture.set(mouse.current_target, pointer_id);
          Effect.Many([
            signal(MakeActive),
            inject(Perform(Move(Point(loc(mouse))))),
          ]);
        | 2 => inject(Perform(Select(Smart(2))))
        | 3 => inject(Perform(Select(Smart(3))))
        | _ => failwith("THEN PERISH")
        };
      | _ => Effect.Ignore
      };

    let toggle_button = (e: Pointer.Event.t, pointer_id: int) => {
      MouseState.pointerup(loc(e));
      PointerCapture.release(e.current_target, pointer_id);
      EdgeScroll.stop();
      Effect.Ignore;
    };

    let drag_select_or_hover = (pointer: Pointer.Event.t) => {
      let left_button_held = pointer.buttons land 1 != 0;
      if (!left_button_held && MouseState.is_button_down()) {
        /* Recover from stuck state: buttons bitmask says left is up
         * but MouseState thinks it's down (missed pointerup) */
        MouseState.reset();
        EdgeScroll.stop();
        Effect.Ignore;
      } else {
        let current_loc = loc(pointer);
        switch (pointer) {
        | {button: Left, _}
            when
              left_button_held
              && MouseState.is_button_down()
              && !Point.equals(current_loc, MouseState.get_down_loc()) =>
          let container = container_target(pointer.current_target);
          let pixel_loc = pointer.loc;
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
                inject(Perform(Select(Resize(Point(goal))))),
              );
            },
          );
          inject(Perform(Select(Resize(Point(current_loc)))));
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
        Key.handler(~f=key => {
          /* 1. Check for arrow key escape at boundaries FIRST.
           *    Keyboard.handle_key_event always returns Some for arrows,
           *    so boundary escape must be checked before delegation. */
          switch (key) {
          | {
              key: D("ArrowLeft" | "ArrowUp"),
              shift: Up,
              meta: Up,
              ctrl: Up,
              alt: Up,
              _,
            }
              when
                z.caret == Outer
                && z.relatives.ancestors == []
                && fst(Siblings.neighbors(z.relatives.siblings)) == None =>
            Effect.Many([Effect.Prevent_default, escape(Left)])
          | {
              key: D("ArrowRight" | "ArrowDown"),
              shift: Up,
              meta: Up,
              ctrl: Up,
              alt: Up,
              _,
            }
              when
                z.caret == Outer
                && z.relatives.ancestors == []
                && snd(Siblings.neighbors(z.relatives.siblings)) == None =>
            Effect.Many([Effect.Prevent_default, escape(Right)])
          | _ =>
            /* 2. Normal editor key handling:
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
          @ (display_line_numbers ? ["has-line-numbers"] : []),
        ),
        key_handler_attr,
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
