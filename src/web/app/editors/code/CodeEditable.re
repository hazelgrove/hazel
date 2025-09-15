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
    | ToggleContextMenu
    | DebugConsole(string);

  exception CantReset;

  let can_undo = (action: t) => {
    switch (action) {
    | Perform(action) => Action.is_historic(action)
    | TAB => true
    | ToggleContextMenu => false
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
            context_menu: false,
          }
        | Error(err) => raise(Action.Failure.Exception(err))
      )
      |> Updated.return(
           ~is_edit=Action.is_edit(action),
           ~recalculate=true,
           ~scroll_active={
             switch (action) {
             | Move(_)
             | Select(
                 Resize(_) | Term(_) | Smart(_) | Tile(_) | ToggleFocus |
                 SetFocus(_),
               )
             | Destruct(_)
             | Insert(_)
             | Put_down
             | Buffer(Set(_) | Accept | Clear)
             | Paste(_)
             | Copy
             | Cut
             | Reparse
             | Introduce
             | Dump => true
             | Project(_)
             | Unselect(_)
             | Refractor(_)
             | Select(All) => false
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
    | ToggleContextMenu =>
      {
        ...model,
        context_menu: !model.context_menu,
      }
      |> Updated.return
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
    | {key: D(key), sys: Mac | PC, shift: Down, meta: Up, ctrl: Up, alt: Up}
        when Keyboard.is_f_key(key) =>
      Some(Update.DebugConsole(key))
    | k =>
      Keyboard.handle_key_event(k) |> Option.map(x => Update.Perform(x));

  let handle_key_event = (~selection, model: Model.t, key) => {
    switch (ProjectorView.key_handoff(model.editor, key)) {
    | Some(action) => Some(Update.Perform(Project(action)))
    | None => handle_key_event(~selection, model, key)
    };
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
    Arms.Refractors.all(~font_metrics=globals.font_metrics, ~syntax, z),
  ];

  let pos_str = (~left, ~top, font_metrics: FontMetrics.t) =>
    Printf.sprintf(
      "position: absolute; left: %fpx; top: %fpx;",
      Float.of_int(left) *. font_metrics.col_width,
      Float.of_int(top) *. font_metrics.row_height,
    );

  let context_menu_view =
      (
        ~inject: Update.t => Ui_effect.t(unit),
        ~measured: Haz3lcore.Measured.t,
        ~font_metrics: FontMetrics.t,
        z: Haz3lcore.Zipper.t,
      )
      : Node.t => {
    open Haz3lcore;
    open WebUtil;
    open Node;
    let caret_point = Zipper.Caret.point(measured, z);
    Node.div(
      ~attrs=[
        Attr.classes(["context-menu", "nut-menu"]),
        Attr.create(
          "style",
          pos_str(
            ~left=caret_point.col,
            ~top=caret_point.row + 1,
            font_metrics,
          ),
        ),
      ],
      [
        // div(
        //   ~attrs=[
        //     Attr.on_pointerdown(_ => {
        //       print_endline("onpointerdown biptch");
        //       Effect.Many([
        //         Effect.Stop_propagation,
        //         Effect.Prevent_default,
        //         inject(Perform(Refractor(SetRefProbe))),
        //       ]);
        //     }),
        //   ],
        //   [text("beprobe")],
        // ),
        NutMenu.submenu(
          ~tooltip="",
          ~icon=Node.div([]),
          [
            div_c(
              "group",
              [
                // div_c("name", [text("Probes")]),
                div_c(
                  "contents",
                  [
                    div(
                      ~attrs=[
                        Attr.on_pointerdown(_ => {
                          Effect.Many([
                            Effect.Stop_propagation,
                            Effect.Prevent_default,
                            inject(Perform(Refractor(SetRefProbe))),
                          ])
                        }),
                        clss(["named-menu-item"]),
                      ],
                      [text("Add probe")],
                    ),
                    div(
                      ~attrs=[
                        Attr.on_pointerdown(_ => {
                          Effect.Many([
                            Effect.Stop_propagation,
                            Effect.Prevent_default,
                            inject(Perform(Refractor(InstrumentTerm))),
                          ])
                        }),
                        clss(["named-menu-item"]),
                      ],
                      [text("Add repl")],
                    ),
                    // Widgets.toggle_named("", ~tooltip="probe", true, _ => {
                    //   Effect.Many([
                    //     Effect.Stop_propagation,
                    //     Effect.Prevent_default,
                    //     inject(Perform(Refractor(SetRefProbe))),
                    //   ])
                    // }),
                    // Widgets.toggle_named("", ~tooltip="repl", true, _ => {
                    //   Effect.Many([
                    //     Effect.Stop_propagation,
                    //     Effect.Prevent_default,
                    //     inject(Perform(Refractor(InstrumentTerm))),
                    //   ])
                    // }),
                  ],
                ),
              ],
            ),
          ],
        ),
      ],
    );
  };

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
    let edit_decos =
      selected
        ? deco(
            ~z=model.editor.state.zipper,
            ~syntax=model.editor.syntax,
            ~globals,
          )
          @ (
            model.context_menu
              ? [
                context_menu_view(
                  ~inject,
                  ~measured=model.editor.syntax.measured,
                  ~font_metrics=globals.font_metrics,
                  model.editor.state.zipper,
                ),
              ]
              : []
          )
        : [];
    let refractor_data =
      ProjectorView.Model.mk(
        Id.Map.union(
          (_, _, b) => Some(b),
          model.editor.state.zipper.refractors.map,
          model.editor.state.zipper.refractors.ephemerals,
        ),
        model.editor.syntax.measured,
        model.editor.syntax.term_data,
        model.editor.syntax.selection_ids,
        Indicated.piece(model.editor.state.zipper),
        model.statics.info_map,
        dynamics,
        selected,
      );
    let refractors_model =
      ProjectorView.all_refractors(
        x => inject(Perform(x)),
        signal(MakeActive),
        globals.font_metrics,
        refractor_data,
      );
    let projectors =
      ProjectorView.all(
        x => inject(Perform(x)),
        signal(MakeActive),
        globals.font_metrics,
        ProjectorView.Model.mk(
          model.editor.syntax.projectors,
          model.editor.syntax.measured,
          model.editor.syntax.term_data,
          model.editor.syntax.selection_ids,
          Indicated.piece(model.editor.state.zipper),
          model.statics.info_map,
          dynamics,
          selected,
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
        print_endline(
          "right click detected. stopping prop, preventing default",
        );
        Effect.Many([
          //Effect.Stop_propagation,
          Effect.Prevent_default,
          inject(Perform(Move(Point(loc(mouse))))),
          inject(ToggleContextMenu),
        ]);
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

    let drag_select = (pointer: Pointer.Event.t) =>
      switch (pointer) {
      | {button: Left, _} when MouseState.is_button_down() =>
        inject(Perform(Select(Resize(Point(loc(pointer))))))
      | _ => Effect.Ignore
      };

    Node.div(
      ~attrs=[
        Attr.classes(
          ["cell-item", "code-editor"] @ (selected ? ["selected"] : []),
        ),
        Attr.on_contextmenu(evt =>
          switch (Pointer.Event.mk(evt)) {
          | {button: Right, ctrl: Up, _} =>
            print_endline(
              "right click with no ctrl hold detected. stopping prop, preventing default",
            );
            Effect.Many([Effect.Stop_propagation, Effect.Prevent_default]);
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
