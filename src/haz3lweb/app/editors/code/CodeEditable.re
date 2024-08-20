open Js_of_ocaml;
open Haz3lcore;
open Virtual_dom.Vdom;
type editor_id = string;
open Util;

module Model = CodeWithStatics.Model;

/* A selectable editable code container component with statics and type-directed code completion. */

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Perform(Action.t)
    | Undo
    | Redo
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
      );
    switch (action) {
    | Perform(action) =>
      perform(action, model)
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
         )
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
    | {key: D("Z" | "z"), sys: Mac, shift: Up, meta: Down, ctrl: Up, alt: Up}
    | {key: D("Z" | "z"), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
      Some(Update.Undo)
    | {key: D(key), sys: Mac | PC, shift: Down, meta: Up, ctrl: Up, alt: Up}
        when Keyboard.is_f_key(key) =>
      Some(Update.DebugConsole(key))
    | k =>
      Keyboard.handle_key_event(k) |> Option.map(x => Update.Perform(x));

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

  let get_goal =
      (
        ~font_metrics: FontMetrics.t,
        text_box: Js.t(Dom_html.element),
        e: Js.t(Dom_html.mouseEvent),
      ) => {
    let rect = text_box##getBoundingClientRect;
    let goal_x = float_of_int(e##.clientX);
    let goal_y = float_of_int(e##.clientY);
    Point.{
      row: Float.to_int((goal_y -. rect##.top) /. font_metrics.row_height),
      col:
        Float.(
          to_int(round((goal_x -. rect##.left) /. font_metrics.col_width))
        ),
    };
  };

  let mousedown_overlay = (~globals: Globals.t, ~inject) =>
    Node.div(
      ~attrs=
        Attr.[
          id("mousedown-overlay"),
          on_mouseup(_ => globals.inject_global(SetMousedown(false))),
          on_mousemove(e => {
            let mouse_handler =
              e##.target |> Js.Opt.get(_, _ => failwith("no target"));
            let text_box =
              JsUtil.get_child_with_class(
                mouse_handler##.parentNode
                |> Js.Opt.get(_, _ => failwith(""))
                |> Js.Unsafe.coerce,
                "code-container",
              )
              |> Option.get;
            let goal =
              get_goal(~font_metrics=globals.font_metrics, text_box, e);
            inject(Action.Select(Resize(Goal(Point(goal)))));
          }),
        ],
      [],
    );

  let mousedown_handler = (~globals: Globals.t, ~signal, ~inject, evt) => {
    let goal =
      get_goal(
        ~font_metrics=globals.font_metrics,
        evt##.currentTarget
        |> Js.Opt.get(_, _ => failwith(""))
        |> JsUtil.get_child_with_class(_, "code-container")
        |> Option.get,
        evt,
      );
    switch (JsUtil.ctrl_held(evt), JsUtil.num_clicks(evt)) {
    | (true, _) =>
      Effect.Many([
        signal(MakeActive),
        inject(Action.Move(Goal(Point(goal)))),
        inject(Action.Jump(BindingSiteOfIndicatedVar)),
      ])
    | (false, 1) =>
      /* Note that we only trigger drag mode (set mousedown)
       * when the left mouse button (aka button 0) is pressed */
      Effect.Many(
        (
          JsUtil.mouse_button(evt) == 0
            ? [globals.inject_global(SetMousedown(true))] : []
        )
        @ [signal(MakeActive), inject(Action.Move(Goal(Point(goal))))],
      )
    | (false, n) => inject(Action.Select(Smart(n)))
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
        model: Model.t,
      ) => {
    let edit_decos = {
      module Deco =
        Deco.Deco({
          let editor = model.editor;
          let globals = globals;
          let statics = model.statics;
        });
      Deco.editor(model.editor.state.zipper, selected);
    };
    let projectors =
      ProjectorView.all(
        model.editor.state.zipper,
        ~cached_statics=model.statics,
        ~cached_syntax=model.editor.syntax,
        ~inject=x => inject(Perform(x)),
        ~font_metrics=globals.font_metrics,
      );
    let overlays = edit_decos @ overlays @ [projectors];
    let code_view =
      CodeWithStatics.View.view(~globals, ~overlays, ~sort?, model);
    let mousedown_overlay =
      selected && globals.mousedown
        ? [mousedown_overlay(~globals, ~inject=x => inject(Perform(x)))]
        : [];
    let on_mousedown =
      mousedown_handler(~globals, ~signal, ~inject=x => inject(Perform(x)));
    Node.div(
      ~attrs=[
        Attr.classes(["cell-item"]),
        Attr.classes(["code-editor"]),
        Attr.on_mousedown(on_mousedown),
      ],
      mousedown_overlay @ [code_view],
    );
  };
};
