open Haz3lcore;
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
    | DebugConsole(string);

  exception CantReset;

  let update =
      (~globals: Globals.t, action: t, model: Model.t): Updated.t(Model.t) => {
    let perform = (action: Action.t, model: Model.t) =>
      Editor.Update.update(
        ~common=
          ProjectorInterface.{
            settings: globals.settings.core,
            font_metrics: globals.font_metrics,
            secondary_icons: globals.settings.secondary_icons,
            show_backpack_targets: globals.show_backpack_targets,
            color_highlights: globals.color_highlights,
            statics: model.statics,
            dynamics: model.dynamics,
          },
        ~sort=Exp,
        action,
        model.editor,
      )
      // |> (
      //   fun
      //   | Ok(editor) =>
      //     Model.{
      //       editor,
      //       statics: model.statics,
      //       dynamics: model.dynamics,
      //     }
      //   | Error(err) => raise(Action.Failure.Exception(err))
      // )
      |> (
        editor =>
          Model.{
            editor,
            statics: model.statics,
            dynamics: model.dynamics,
          }
          |> Updated.return(
               ~is_edit=Action.is_edit(action),
               ~recalculate=true,
               ~scroll_active=Action.should_scroll_active(action),
             )
      );
    switch (action) {
    | Perform(action) => perform(action, model)
    | DebugConsole(key) =>
      DebugConsole.print(~settings=globals.settings, model, key);
      model |> Updated.return_quiet;
    | TAB =>
      /* Attempt to act intelligently when TAB is pressed.
       * TODO: Consider more advanced TAB logic. Instead
       * of simply moving to next hole, if the backpack is non-empty
       * but can't immediately put down, move to next position of
       * interest, which is closet of: nearest position where can
       * put down, farthest position where can put down, next hole */
      let z = model.editor |> Editor.Model.get_z;
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
  type t = Editor.Focus.t;

  let get_cursor_info = (~selection as _, model: Model.t): cursor(Update.t) => {
    {
      ...
        CodeWithStatics.Model.get_cursor_info(model)
        |> map(x => Update.Perform(x)),
      editor_read_only: false,
      undo_action: None,
      redo_action: None,
    };
  };

  let handle_key_event =
      (~selection, model: Model.t): (Key.t => option(Update.t)) =>
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
      None
    | {key: D("Tab"), sys: _, shift: Up, meta: Up, ctrl: Up, alt: Up} =>
      Some(Update.TAB)
    | {key: D("Z" | "z"), sys: Mac, shift: Up, meta: Down, ctrl: Up, alt: Up}
    | {key: D("Z" | "z"), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
      None
    | {key: D(key), sys: Mac | PC, shift: Down, meta: Up, ctrl: Up, alt: Up}
        when Keyboard.is_f_key(key) =>
      Some(Update.DebugConsole(key))
    | k =>
      Editor.Focus.handle_key_event(~focus=selection, ~key=k, model.editor)
      |> Option.map(x => Update.Perform(x));

  // let handle_key_event = (~selection, model: Model.t, key) => {
  //   //TODO(andrew): not sure handoff approach makes sense
  //   switch (Editor.Update.key_handoff(model.editor, key)) {
  //   | Some(action) => Some(Update.Perform(Project(action)))
  //   | None => handle_key_event(~selection, model, key)
  //   };
  // };

  let jump_to_tile = (tile, model: Model.t) => {
    Editor.Update.jump_to_tile_action(tile, model.editor)
    |> Option.map(x => Update.Perform(x));
  };
};

// module View = {
//   type event =
//     | MakeActive;

//   let container_target = (current_target: Js.opt(Js.t(Dom_html.element))) =>
//     current_target
//     |> Js.Opt.get(_, _ => failwith(""))
//     |> JsUtil.get_child_with_class(_, "code-container")
//     |> Option.get;

//   module PointerCapture = {
//     /* This uses the Pointer Capture API to keep mouse movement data flowing
//      * to an editor even when the mouse exits the editor element or even
//      * browser window. This is necessary to (for example) be able to select
//      * upwards while auto-scrolling the editor by flinging your mouse to the
//      * top of your screen; otherwise, the selection action stops as the
//      * mouse exits the editor element's bounding box. */

//     let set = (target, pointer_id) =>
//       JsUtil.setPointerCapture(container_target(target), pointer_id);

//     let release = (target, pointer_id) =>
//       if (JsUtil.hasPointerCapture(container_target(target), pointer_id)) {
//         JsUtil.releasePointerCapture(container_target(target), pointer_id);
//       };
//   };

//   module MouseState = Pointer.MkState();

//   let view =
//       (
//         ~globals: Globals.t,
//         ~signal: event => Ui_effect.t(unit),
//         ~inject: Update.t => Ui_effect.t(unit),
//         ~selected: bool,
//         ~overlays: list(Node.t)=[],
//         ~sort=?,
//         model: Model.t,
//       ) => {
//     let edit_decos = {
//       module Deco =
//         Deco.Deco({
//           type projector = Projector.Model.t;
//           type projector_kind = ProjectorCore.Kind.t;
//           let editor = model.editor;
//           let globals =
//             ProjectorInterface.{
//               settings: globals.settings.core,
//               font_metrics: globals.font_metrics,
//               secondary_icons: globals.settings.secondary_icons,
//               show_backpack_targets: globals.show_backpack_targets,
//               color_highlights: globals.color_highlights,
//               statics: model.statics,
//               dynamics: model.dynamics,
//             };
//         });
//       Deco.editor(model.editor |> Editor.Model.get_z, selected);
//     };
//     let projectors =
//       Editor.View.all_projectors(
//         ~settings=globals.settings.core,
//         ~font_metrics=globals.font_metrics,
//         ~secondary_icons=globals.settings.secondary_icons,
//         ~inject=x => inject(Perform(x)),
//         ~make_active=signal(MakeActive),
//         ~statics=model.statics,
//         Editor.View.mk_projector_model(
//           model.editor |> Editor.get_projectors,
//           model.editor |> Editor.get_measured,
//           model.editor |> Editor.get_selection_ids,
//           model.editor |> Editor.get_indicated,
//           model.statics.info_map,
//           model.dynamics,
//           selected,
//         ),
//       );
//     let overlays =
//       [Node.div(~attrs=[Attr.classes(["code-deco"])], edit_decos)]
//       @ [Node.div(~attrs=[Attr.classes(["overlays"])], overlays)]
//       @ projectors;
//     let code_view =
//       EditorView.view_code_statics(
//         ~globals,
//         ~overlays,
//         ~sort?,
//         ~statics=model.statics,
//         ~dynamics=model.dynamics,
//         model.editor,
//       );

//     let loc = (e: Pointer.Event.t) =>
//       FontMetrics.get_goal(
//         ~font_metrics=globals.font_metrics,
//         container_target(e.current_target),
//         e.loc,
//       );

//     let move_or_select = (mouse: Pointer.Event.t, pointer_id: int) =>
//       switch (mouse) {
//       | {shift: Down, _} =>
//         Effect.Many([
//           signal(MakeActive),
//           inject(Perform(Select(Resize(Goal(Point(loc(mouse))))))),
//         ])
//       | {sys: PC, ctrl: Down, _}
//       | {sys: Mac, meta: Down, _} =>
//         Effect.Many([
//           signal(MakeActive),
//           inject(Perform(Move(Goal(Point(loc(mouse)))))),
//           inject(Perform(Jump(BindingSiteOfIndicatedVar))),
//         ])
//       | {button: Left, _} =>
//         MouseState.pointerdown(loc(mouse));
//         let click_count = MouseState.count();
//         /* Check how many clicks have happened recently
//          * and cycle between options on-click */
//         switch (click_count mod 3 + 1) {
//         | 1 =>
//           /* prepare to drag if the mouse moves */
//           PointerCapture.set(mouse.current_target, pointer_id);
//           Effect.Many([
//             signal(MakeActive),
//             inject(Perform(Move(Goal(Point(loc(mouse)))))),
//           ]);
//         | 2 => inject(Perform(Select(Smart(2))))
//         | 3 => inject(Perform(Select(Smart(3))))
//         | _ => failwith("THEN PERISH")
//         };
//       | _ => Effect.Ignore
//       };

//     let toggle_button = (e: Pointer.Event.t, pointer_id: int) => {
//       MouseState.pointerup(loc(e));
//       PointerCapture.release(e.current_target, pointer_id);
//       Effect.Ignore;
//     };

//     let drag_select = (pointer: Pointer.Event.t) =>
//       switch (pointer) {
//       | {button: Left, _} when MouseState.is_button_down() =>
//         inject(Perform(Select(Resize(Goal(Point(loc(pointer)))))))
//       | _ => Effect.Ignore
//       };

//     Node.div(
//       ~attrs=[
//         Attr.classes(
//           ["cell-item", "code-editor"] @ (selected ? ["selected"] : []),
//         ),
//         Attr.on_pointerdown(evt =>
//           move_or_select(Pointer.Event.mk(evt), Pointer.Event.id_of(evt))
//         ),
//         Attr.on_pointerup(evt =>
//           toggle_button(Pointer.Event.mk(evt), Pointer.Event.id_of(evt))
//         ),
//         Attr.on_mousemove(evt => drag_select(Pointer.Event.mk(evt))),
//         Attr.on_wheel(evt => drag_select(Pointer.Event.mk(evt))),
//       ],
//       [code_view],
//     );
//   };
// };
