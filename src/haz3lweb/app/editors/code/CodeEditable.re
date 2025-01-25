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

  let container_target = evt =>
    evt##.currentTarget
    |> Js.Opt.get(_, _ => failwith(""))
    |> JsUtil.get_child_with_class(_, "code-container")
    |> Option.get;

  module StateMachine = {
    /* State Machine Diagram:
     *
     *        down=>Move      up=>SetTimer     down=>SelectToken    up=>SetTimer
     * Up(One) ------> Down(One) --------> Up(Two) -------> Down(Two) -----> Up(Three)
     *   ^                                                                       |
     *   |                           down=>SelectTerm                            |
     *   +-----------------------------------------------------------------------+
     *
     * BASICS:
     * - We start in Up(One)
     * - Pointerdown transitions emit actions: Move, SelectToken, or SelectTerm
     * - Pointerup transitions start timers that auto-reset to Up(One) after delay
     * - Being in Down(_) states enables drag selection
     *
     * DETAILS:
     *   This models the click state of an editor. It models a pointer as
     *   being in an alternating sequence of up and down states, beginning
     *   on (and returning to) Up(One). A pointerdown event transitions from
     *   an up state to a down state, and vice versa for pointerup. Furthermore,
     *   a pointerdown transition produces an action to execute, and a pointerup
     *   transition introduces a state transition timer, which is used to decide
     *   whether consecutive up/down cycles (clicks) constitute individual clicks
     *   or double/triple-clicks. The former induces caret-to-cursor movement;
     *   the latter moves and then also selects token (double) or term (triple).
     *   This is manually implemented as a state machine as the
     *   multi-click detection intersect awkwardly. */

    [@deriving (show({with_path: false}), sexp, yojson)]
    type iter =
      | One
      | Two
      | Three;

    [@deriving (show({with_path: false}), sexp, yojson)]
    type state =
      | Up(iter)
      | Down(iter);

    [@deriving (show({with_path: false}), sexp, yojson)]
    type action =
      | Move
      | SelectToken
      | SelectTerm;

    [@deriving (show({with_path: false}), sexp, yojson)]
    type timer = option((state, state));

    let state: ref(state) = ref(Up(One));

    let should_drag_select = (): bool => {
      switch (state^) {
      | Up(_) => false
      | Down(_) => true
      };
    };

    let down = (old_state): (state, action) =>
      switch (old_state) {
      | Up(One) => (Down(One), Move)
      | Up(Two) => (Down(Two), SelectToken)
      | Up(Three) => (Down(Three), SelectTerm)
      | Down(_) => failwith("THEN PERISH")
      };

    let up = (old_state): (state, timer) =>
      switch (old_state) {
      | Down(One) => (Up(Two), Some((Up(Two), Up(One))))
      | Down(Two) => (Up(Three), Some((Up(Three), Up(One))))
      | Down(Three) => (Up(One), None)
      | Up(_) => failwith("YOU SHOULD NOT BE")
      };

    let down_transition = (): action => {
      let (new_state, action) = down(state^);
      state := new_state;
      action;
    };

    let up_transition = () => {
      let (new_state, timer) = up(state^);
      state := new_state;
      switch (timer) {
      | None => ()
      | Some((old, next)) =>
        let delay_ms = 310.0;
        JsUtil.delay(delay_ms, () =>
          if (old == state^) {
            state := next;
          }
        );
      };
    };
  };

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

    let goal = evt =>
      FontMetrics.get_goal(
        ~font_metrics=globals.font_metrics,
        container_target(evt),
        evt,
      );

    let move_or_select = evt =>
      if (JsUtil.shift_held(evt)) {
        /* If we're holding shift, range select from current to indicated */
        Effect.Many([
          signal(MakeActive),
          inject(Perform(Select(Resize(Goal(Point(goal(evt))))))),
        ]);
      } else if (JsUtil.ctrl_held(evt) || Os.is_mac^ && JsUtil.meta_held(evt)) {
        /* If we're holding ctrl/cmd, jump to indicated variable's binding */
        Effect.Many([
          signal(MakeActive),
          inject(Perform(Move(Goal(Point(goal(evt)))))),
          inject(Perform(Jump(BindingSiteOfIndicatedVar))),
        ]);
      } else {
        /* Otherwise, either move or select token/term, depending on state */
        switch (StateMachine.down_transition()) {
        | Move =>
          Effect.Many([
            signal(MakeActive),
            inject(Perform(Move(Goal(Point(goal(evt)))))),
          ])
        | SelectToken => inject(Perform(Select(Smart(2))))
        | SelectTerm => inject(Perform(Select(Smart(3))))
        };
      };

    let toggle_mode = _evt => {
      StateMachine.up_transition();
      Effect.Ignore;
    };

    let drag_select = evt => {
      StateMachine.should_drag_select() && JsUtil.mouse_button(evt) == 0
        ? inject(Perform(Select(Resize(Goal(Point(goal(evt)))))))
        : Effect.Ignore;
    };

    Node.div(
      ~attrs=[
        Attr.classes(
          ["cell-item", "code-editor"] @ (selected ? ["selected"] : []),
        ),
        Attr.on_pointerdown(move_or_select),
        Attr.on_pointerup(toggle_mode),
        Attr.on_mousemove(drag_select),
      ],
      [code_view],
    );
  };
};
