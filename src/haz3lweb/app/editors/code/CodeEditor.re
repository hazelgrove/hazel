open Js_of_ocaml;
open Haz3lcore;
open Virtual_dom.Vdom;
type editor_id = string;
open Sexplib.Std;

module Model = ReadOnlyEditor.Model;

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Perform(Action.t)
    | Undo
    | Redo
    | Reparse
    | Assistant(UpdateAssistant.t)
    | DebugConsole(string);

  exception CantReset;

  let update =
      (~settings: Settings.t, action: t, model: Model.t): Updated.t(Model.t) => {
    let perform = (action, model: Model.t) =>
      Perform.go(~settings=settings.core, action, model.editor)
      |> (
        fun
        | Ok(editor) => Model.{editor, statics: model.statics}
        | Error(err) => raise(Action.Failure.Exception(err))
      );
    let perform_all =
      List.fold_left((model, action) => perform(action, model));
    switch (action) {
    | Perform(action) =>
      perform(action, model)
      |> Updated.return(
           ~is_edit=Action.is_edit(action),
           ~recalculate=Action.is_edit(action),
           ~scroll_active={
             switch (action) {
             | Move(_)
             | MoveToNextHole(_)
             | Jump(_)
             | Select(Resize(_) | Term(_) | Smart | Tile(_))
             | Destruct(_)
             | Insert(_)
             | Pick_up
             | Put_down
             | RotateBackpack
             | MoveToBackpackTarget(_)
             | Paste(_) => true
             | Unselect(_)
             | Select(All)
             | Suggest(_)
             | ResetSuggestion => false
             };
           },
         )
    | Undo =>
      switch (Editor.undo(model.editor)) {
      | Some(editor) =>
        Model.{editor, statics: model.statics} |> Updated.return
      | None => model |> Updated.return_quiet
      }
    | Redo =>
      switch (Editor.redo(model.editor)) {
      | Some(editor) =>
        Model.{editor, statics: model.statics} |> Updated.return
      | None => model |> Updated.return_quiet
      }
    | Reparse =>
      let zipper_init = Zipper.init();
      let ed_str = Printer.to_string_editor(model.editor);
      switch (Printer.zipper_of_string(~zipper_init, ed_str)) {
      | None => raise(CantReset)
      | Some(z) =>
        //TODO: add correct action to history (Pick_up is wrong)
        let* editor =
          Haz3lcore.Editor.new_state(Pick_up, z, model.editor)
          |> Updated.return;
        Model.{editor, statics: model.statics};
      };
    | Assistant(a) =>
      perform_all(
        model,
        UpdateAssistant.assistant_action_to_editor_actions(
          ~settings=settings.core,
          model.editor,
          a,
        ),
      )
      |> (
        switch (a) {
        | Prompt(_) => Updated.return_quiet(_)
        | AcceptSuggestion => Updated.return(_)
        }
      )
    | DebugConsole(key) =>
      DebugConsole.print(~settings, model.editor, key);
      model |> Updated.return_quiet;
    };
  };

  let calculate = (~settings, ~stitch, model: Model.t): Model.t =>
    if (DHExp.fast_equal(
          MakeTerm.from_zip_for_sem(model.editor.state.zipper) |> fst,
          model.statics.term,
        )) {
      model;
    } else {
      Model.mk_from_editor(~settings, ~stitch, model.editor);
    };
};

module Selection = {
  let get_cursor_info = (model: Model.t) => {
    Indicated.ci_of(model.editor.state.zipper, model.statics.info_map);
  };

  let handle_key_event = (model: Model.t): (Key.t => option(Update.t)) =>
    fun
    | {key: D("b"), sys: Mac | PC, shift: Up, meta: Down, ctrl: Up, alt: Up} =>
      Some(Update.Reparse)
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
    | {key: D("k"), sys: Mac | PC, shift: Up, meta: Down, ctrl: Down, alt: Up} =>
      Some(Update.Reparse)
    | {key: D("Tab"), sys: Mac | PC, shift: Up, meta: Down, ctrl: Up, alt: Up} =>
      /* Attempt to act intelligently when TAB is pressed.
       * TODO(andrew): Consider more advanced TAB logic. Instead
       * of simply moving to next hole, if the backpack is non-empty
       * but can't immediately put down, move to next position of
       * interest, which is closet of: nearest position where can
       * put down, farthest position where can put down, next hole */
      Selection.is_buffer(model.editor.state.zipper.selection)
        ? Some(Update.Assistant(AcceptSuggestion))
        : Zipper.can_put_down(model.editor.state.zipper)
            ? Some(Update.Perform(Put_down))
            : Some(Update.Perform(MoveToNextHole(Right)))
    | {key: D("/"), sys: Mac | PC, shift: Up, meta: Down, ctrl: Up, alt: Up} =>
      Some(Assistant(Prompt(TyDi)))
    | {key: D(key), sys: Mac | PC, shift: Down, meta: Up, ctrl: Up, alt: Up}
        when Keyboard.is_f_key(key) =>
      Some(Update.DebugConsole(key))
    | k =>
      Keyboard.handle_key_event(k) |> Option.map(x => Update.Perform(x));

  let jump_to_tile = (tile, model: Model.t) => {
    switch (TileMap.find_opt(tile, model.editor.state.meta.tiles)) {
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
    Measured.Point.{
      row: Float.to_int((goal_y -. rect##.top) /. font_metrics.row_height),
      col:
        Float.(
          to_int(round((goal_x -. rect##.left) /. font_metrics.col_width))
        ),
    };
  };

  let mousedown_overlay = (~globals: Globals.t, ~inject) =>
    Node.div(
      ~attr=
        Attr.many(
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
        ),
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
      Effect.Many([
        globals.inject_global(SetMousedown(true)),
        signal(MakeActive),
        inject(Action.Move(Goal(Point(goal)))),
      ])
    | (false, 2) => inject(Action.Select(Tile(Current)))
    | (false, 3 | _) => inject(Action.Select(Smart))
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
        });
      Deco.editor(
        model.editor.state.zipper,
        model.editor.state.meta.segment,
        selected,
      )
      @ (
        switch (globals.color_highlights) {
        | Some(colorMap) =>
          Deco.color_highlights(ColorSteps.to_list(colorMap))
        | _ => []
        }
      );
    };
    let overlays = edit_decos @ overlays;
    let code_view =
      ReadOnlyEditor.View.view(~globals, ~overlays, ~sort?, model);
    let mousedown_overlay =
      selected && globals.mousedown
        ? [mousedown_overlay(~globals, ~inject=x => inject(Perform(x)))]
        : [];
    let on_mousedown =
      mousedown_handler(~globals, ~signal, ~inject=x => inject(Perform(x)));
    let paste_handler =
      selected
        ? [
          Attr.on_paste(evt => {
            let pasted_text =
              Js.to_string(evt##.clipboardData##getData(Js.string("text")))
              |> Str.global_replace(Str.regexp("\n[ ]*"), "\n");
            Dom.preventDefault(evt);
            inject(Update.Perform(Paste(pasted_text)));
          }),
        ]
        : [];
    let copy_handler =
      selected
        ? {
          let selection = Printer.to_string_selection(model.editor);
          [
            Attr.on_copy(_ => {
              JsUtil.copy(selection);
              Effect.Ignore;
            }),
            Attr.on_cut(_ => {
              JsUtil.copy(selection);
              inject(Update.Perform(Destruct(Left)));
            }),
          ];
        }
        : [];
    Node.div(
      ~attr=
        Attr.many(
          [
            Attr.classes(["cell-item"]),
            Attr.classes(["code-editor"]),
            Attr.on_mousedown(on_mousedown),
          ]
          @ paste_handler
          @ copy_handler,
        ),
      mousedown_overlay @ [code_view],
    );
  };
};

let view = View.view;
