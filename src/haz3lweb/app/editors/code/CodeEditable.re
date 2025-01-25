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
    | Undo
    | Redo
    | TAB
    | DebugConsole(string);

  exception CantReset;

  let update =
      (~settings: Settings.t, action: t, model: Model.t): Updated.t(Model.t) => {
    let perform = (action, model: Model.t) =>
      Editor.Update.update(
        ~settings=settings.core,
        action,
        model.statics,
        model.editor,
      )
      |> (
        fun
        | Ok(editor) => Model.{editor, statics: model.statics}
        | Error(err) => raise(Action.Failure.Exception(err))
      )
      |> Updated.return(
           ~is_edit=Action.is_edit(action),
           ~recalculate=true,
           ~scroll_active={
             switch (action) {
             | Move(_)
             | Jump(_)
             | Select(Resize(_) | Term(_) | Smart(_) | Tile(_))
             | Destruct(_)
             | Insert(_)
             | Pick_up
             | Put_down
             | RotateBackpack
             | MoveToBackpackTarget(_)
             | Buffer(Set(_) | Accept | Clear)
             | Paste(_)
             | Copy
             | Cut
             | Reparse => true
             | Project(_)
             | Unselect(_)
             | Select(All) => false
             };
           },
         );
    switch (action) {
    | Perform(action) => perform(action, model)
    | Undo =>
      switch (Editor.Update.undo(model.editor)) {
      | Some(editor) => Model.{...model, editor} |> Updated.return
      | None => model |> Updated.return_quiet
      }
    | Redo =>
      switch (Editor.Update.redo(model.editor)) {
      | Some(editor) => Model.{...model, editor} |> Updated.return
      | None => model |> Updated.return_quiet
      }
    | DebugConsole(key) =>
      DebugConsole.print(~settings, model, key);
      model |> Updated.return_quiet;
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
              ? Put_down : Move(Goal(Piece(Grout, Right)));
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
      undo_action: Some(Update.Undo),
      redo_action: Some(Update.Redo),
    };
  };

  let handle_key_event =
      (~selection as (), _: Model.t): (Key.t => option(Update.t)) =>
    fun
    | {
        key: D("Z" | "z"),
        sys: Mac,
        shift: Down,
        meta: Down,
        ctrl: Up,
        alt: Up,
      }
    | {
        key: D("Z" | "z"),
        sys: PC,
        shift: Down,
        meta: Up,
        ctrl: Down,
        alt: Up,
      } =>
      Some(Update.Redo)
    | {key: D("Tab"), sys: _, shift: Up, meta: Up, ctrl: Up, alt: Up} =>
      Some(Update.TAB)
    | {key: D("Z" | "z"), sys: Mac, shift: Up, meta: Down, ctrl: Up, alt: Up}
    | {key: D("Z" | "z"), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
      Some(Update.Undo)
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

  let jump_to_tile = (tile, model: Model.t) => {
    switch (TileMap.find_opt(tile, model.editor.syntax.tiles)) {
    | Some(_) => Some(Update.Perform(Jump(TileId(tile))))
    | None => None
    };
  };
};

module View = {
  type event =
    | MakeActive;

  /* A sidechannel for the id of the pointer when capturing pointer events for
   * drag-based selection. This is necessary to sidechannel the pointer id
   * as our libraries currently don't support the on_pointer_move handler.
   * We also use on_click (another mouse-only handler) to track multiple
   * clicks for token/term selection, but this logic could be moved in-house;
   * it's already half in-house as can be seen in the double_click_flag below */
  let drag_pointer: ref(option(int)) = ref(None);
  /* This flag supports double/triple click to select token/term
   * behavior. Without this flag, there will be a moment between the
   * second and third click where the selection disappears; the behavior
   * is still essentially the same, but it is visually distracting.
   *
   * The pointerdown and on_click events fire alternatingly in that order.
   * i.e. a singleclick is pointerdown => on_click and a tripleclick is
   * pointerdown => on_click => pointerdown => on_click => pointerdown => on_click
   *
   * We want pointerdown to do caret movement (waiting for pointerup feels laggy).
   * However we don't want to (re)do caret movement if we've already made a token
   * selection by double-clicking as this would break the selection. So we set
   * a flag on click, and if this flag is true on the subsequent pointerdown,
   * we no-op. However, we need to make sure this flag ultimately gets reset;
   * we can't rely on their necessarily being any following pointerdowns/clicks,
   * so we set a timer to reset it */
  let multi_click_flag: ref(bool) = ref(false);

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: bool,
        ~overlays: list(Node.t)=[],
        ~sort=?,
        ~dynamics: Dynamics.Map.t,
        model: Model.t,
      ) => {
    let edit_decos = {
      module Deco =
        Deco.Deco({
          let editor = model.editor;
          let globals = globals;
          let statics = model.statics;
          let dynamics = dynamics;
        });
      Deco.editor(model.editor.state.zipper, selected);
    };
    let projectors =
      ProjectorView.all(
        x => inject(Perform(x)),
        globals.font_metrics,
        ProjectorView.collect_data(
          model.editor.syntax,
          model.editor.state.zipper,
          model.statics,
          dynamics,
        ),
      );
    let overlays =
      [Node.div(~attrs=[Attr.classes(["code-deco"])], edit_decos)]
      @ [Node.div(~attrs=[Attr.classes(["overlays"])], overlays)]
      @ projectors;
    let code_view =
      CodeWithStatics.View.view(
        ~globals,
        ~overlays,
        ~dynamics,
        ~sort?,
        model,
      );

    let container_target = evt =>
      evt##.currentTarget
      |> Js.Opt.get(_, _ => failwith(""))
      |> JsUtil.get_child_with_class(_, "code-container")
      |> Option.get;

    let get_goal = evt =>
      FontMetrics.get_goal(
        ~font_metrics=globals.font_metrics,
        container_target(evt),
        evt,
      );

    let set_drag = evt => {
      drag_pointer := Some(evt##.pointerId);
      JsUtil.setPointerCapture(container_target(evt), evt##.pointerId);
    };

    let release_drag = evt =>
      switch (drag_pointer^) {
      | Some(pid) =>
        drag_pointer := None;
        let target = container_target(evt);
        if (JsUtil.hasPointerCapture(target, pid)) {
          JsUtil.releasePointerCapture(target, pid);
        };
      | None => ()
      };

    Node.div(
      ~attrs=[
        Attr.classes(
          ["cell-item", "code-editor"] @ (selected ? ["selected"] : []),
        ),
        Attr.on_pointerdown(evt =>
          if (JsUtil.shift_held(evt)) {
            Effect.Many([
              signal(MakeActive),
              inject(
                Perform(Select(Resize(Goal(Point(get_goal(evt)))))),
              ),
            ]);
          } else if (JsUtil.ctrl_held(evt)
                     || Os.is_mac^
                     && JsUtil.meta_held(evt)) {
            Effect.Many([
              signal(MakeActive),
              inject(Perform(Move(Goal(Point(get_goal(evt)))))),
              inject(Perform(Jump(BindingSiteOfIndicatedVar))),
            ]);
          } else if (multi_click_flag^) {
            set_drag(evt);
            Effect.Ignore;
          } else {
            set_drag(evt);
            Effect.Many([
              signal(MakeActive),
              inject(Perform(Move(Goal(Point(get_goal(evt)))))),
            ]);
          }
        ),
        Attr.on_click(evt => {
          multi_click_flag := true;
          JsUtil.delay(400.0, () => {multi_click_flag := false});
          release_drag(evt);
          switch (JsUtil.num_clicks(evt)) {
          | 1 => Effect.Ignore
          | n => inject(Perform(Select(Smart(n))))
          };
        }),
        Attr.on_mousemove(evt =>
          switch (drag_pointer^) {
          | Some(_) when JsUtil.mouse_button(evt) == 0 =>
            /* Only drag for button 0 (left mouse button) */
            inject(Perform(Select(Resize(Goal(Point(get_goal(evt)))))))
          | _ => Effect.Ignore
          }
        ),
      ],
      [code_view],
    );
  };
};
