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
             | Probe(StepInto(_))
             | Dump => true
             | Project(_)
             | Unselect(_)
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
      {
        ...model,
        context_menu: ContextMenu.Model.update(action, model.context_menu),
      }
      |> Updated.return_quiet
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
          : Zipper.can_put_down(z) ? Put_down : Move(Goal(Hole(Right)));
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

  let get_cursor_info = (~selection as (), model: Model.t): cursor(Update.t) => {
    {
      ...
        CodeWithStatics.Model.get_cursor_info(model)
        |> map(x => Update.Perform(x)),
      editor_read_only: false,
    };
  };

  let handle_key_event =
      (~selection as (), _: Model.t): (Key.t => option(Update.t)) =>
    fun
    | {key: D("Tab"), sys: _, shift: Up, meta: Up, ctrl: Up, alt: Up} =>
      Some(Update.TAB)
    /* Cmd+. (Mac) / Ctrl+. (PC) opens context menu - VS Code Quick Fix convention */
    | {key: D("."), sys: Mac, shift: Up, meta: Down, ctrl: Up, alt: Up}
    | {key: D("."), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
      Some(Update.ContextMenu(ContextMenu.Model.Open))
    /* Shift+F10 opens context menu (VS Code convention) */
    | {key: D("F10"), sys: _, shift: Down, meta: Up, ctrl: Up, alt: Up} =>
      Some(Update.ContextMenu(ContextMenu.Model.Open))
    | {key: D(key), sys: Mac | PC, shift: Down, meta: Up, ctrl: Up, alt: Up}
        when Keyboard.is_f_key(key) =>
      Some(Update.DebugConsole(key))
    | k =>
      Keyboard.handle_key_event(k) |> Option.map(x => Update.Perform(x));

  let handle_key_event = (~selection, model: Model.t, key: Key.t) =>
    /* Handle context menu keyboard navigation */
    switch (model.context_menu) {
    | Some(selected_index) =>
      switch (key.key) {
      | D("Escape") => Some(Update.ContextMenu(ContextMenu.Model.Close))
      | D("ArrowUp") => Some(Update.ContextMenu(ContextMenu.Model.Up))
      | D("ArrowDown") => Some(Update.ContextMenu(ContextMenu.Model.Down))
      | D("Enter") =>
        /* Get the action at the selected index and execute it */
        let action =
          ContextMenu.get_action_at_index(
            ~info_map=model.statics.info_map,
            model.editor.state.zipper,
            selected_index,
          );
        switch (action) {
        | Some(action) => Some(Update.Perform(action)) /* Action will close menu */
        | None => Some(Update.ContextMenu(ContextMenu.Model.Close))
        };
      | _ =>
        switch (ProjectorView.key_handoff(model.editor, key)) {
        | Some(action) => Some(Update.Perform(Project(action)))
        | None => handle_key_event(~selection, model, key)
        }
      }
    | None =>
      switch (ProjectorView.key_handoff(model.editor, key)) {
      | Some(action) => Some(Update.Perform(Project(action)))
      | None => handle_key_event(~selection, model, key)
      }
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
    |> Option.get;

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

  let deco = (~syntax: CachedSyntax.t, ~z: Zipper.t, ~globals: Globals.t) => [
    CaretDec.view(
      ~measured=syntax.measured,
      ~font_metrics=globals.font_metrics,
      z,
    ),
    Arms.Indicated.term(~font_metrics=globals.font_metrics, ~syntax, z),
    Highlight.selection(
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
  ];

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: bool,
        ~overlays: list(Node.t)=[],
        ~dynamics: Language.Dynamics.Map.t,
        model: Model.t,
      ) => {
    /* Sync document-level click listener for closing context menu */
    ContextMenuListener.sync(
      selected && Model.context_menu_is_open(model),
      inject(ContextMenu(ContextMenu.Model.Close)),
    );
    let edit_decos =
      selected
        ? deco(
            ~z=model.editor.state.zipper,
            ~syntax=model.editor.syntax,
            ~globals,
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
            | Some(selected_index) => [
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
                  ~syntax=model.editor.syntax,
                  ~info_map=model.statics.info_map,
                  ~font_metrics=globals.font_metrics,
                  ~selected_index,
                  model.editor.state.zipper,
                ),
              ]
            | None => []
            }
          )
        : [];
    let zipper = model.editor.state.zipper;
    let refractor_data =
      RefractorView.mk_data(
        ~refractors=
          Id.Map.union(
            (_, _, b) => Some(b),
            zipper.refractors.manuals,
            zipper.refractors.autos.ephemerals,
          ),
        ~syntax=model.editor.syntax,
        ~indicated=Indicated.piece(zipper),
        ~statics=model.statics.info_map,
        ~dynamics,
        ~sample_cursor=zipper.refractors.sample_cursor,
        ~editor_active=selected,
      );
    let visible = globals.visible_rows;
    let refractors_model =
      RefractorView.all(
        x => inject(Perform(x)),
        signal(MakeActive),
        globals.font_metrics,
        ~visible?,
        refractor_data,
      );
    let projectors =
      ProjectorView.all(
        x => inject(Perform(x)),
        signal(MakeActive),
        globals.font_metrics,
        ~visible?,
        ProjectorView.Model.mk(
          ~syntax=model.editor.syntax,
          ~indicated=Indicated.piece(zipper),
          ~statics=model.statics.info_map,
          ~dynamics,
          ~sample_cursor=zipper.refractors.sample_cursor,
          ~editor_active=selected,
        ),
      );
    let overlays =
      [Node.div(~attrs=[Attr.classes(["code-deco"])], edit_decos)]
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
      Effect.Ignore;
    };

    let drag_select = (pointer: Pointer.Event.t) => {
      let current_loc = loc(pointer);
      switch (pointer) {
      | {button: Left, _}
          when
            MouseState.is_button_down()
            && !Point.equals(current_loc, MouseState.get_down_loc()) =>
        inject(Perform(Select(Resize(Point(current_loc)))))
      | _ => Effect.Ignore
      };
    };

    Node.div(
      ~attrs=[
        Attr.classes(
          ["cell-item", "code-editor"] @ (selected ? ["selected"] : []),
        ),
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
        Attr.on_mousemove(evt => drag_select(Pointer.Event.mk(evt))),
        Attr.on_wheel(evt => drag_select(Pointer.Event.mk(evt))),
      ],
      [code_view],
    );
  };
};
