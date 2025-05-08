open Js_of_ocaml;
open Haz3lcore;
open Virtual_dom.Vdom;
type editor_id = string;
open Util;

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

let view_code_statics =
    (
      ~globals: Globals.t,
      ~overlays: list(Node.t)=[],
      ~sort=Sort.root,
      ~statics,
      ~dynamics,
      editor,
    ) => {
  let code_text_view =
    Editor.View.view(
      ~secondary_icons=globals.settings.secondary_icons,
      ~font_metrics=globals.font_metrics,
      ~sort,
      editor,
    );
  let statics_decos = {
    module Deco =
      Deco.Deco({
        type projector_kind = ProjectorCore.Kind.t;
        type projector = Projector.Model.t;
        let globals =
          ProjectorInterface.{
            settings: globals.settings.core,
            font_metrics: globals.font_metrics,
            secondary_icons: globals.settings.secondary_icons,
            show_backpack_targets: globals.show_backpack_targets,
            color_highlights: globals.color_highlights,
            statics,
            dynamics,
          };
        let editor = editor;
      });
    Deco.statics();
  };
  Web.div_c("code-container", [code_text_view] @ statics_decos @ overlays);
};

let view_code_editable =
    (
      ~globals: Globals.t,
      ~signal: event => Ui_effect.t(unit),
      ~inject: Action.t => Ui_effect.t(unit),
      ~selected: bool,
      ~overlays: list(Node.t)=[],
      ~sort=?,
      ~statics: CachedStatics.t,
      ~dynamics: Dynamics.Map.t,
      model: Editor.Model.t,
    ) => {
  let edit_decos = {
    module Deco =
      Deco.Deco({
        type projector = Projector.Model.t;
        type projector_kind = ProjectorCore.Kind.t;
        let editor = model;
        let globals =
          ProjectorInterface.{
            settings: globals.settings.core,
            font_metrics: globals.font_metrics,
            secondary_icons: globals.settings.secondary_icons,
            show_backpack_targets: globals.show_backpack_targets,
            color_highlights: globals.color_highlights,
            statics,
            dynamics,
          };
      });
    Deco.editor(model |> Editor.Model.get_z, selected);
  };
  let projectors =
    Editor.View.all_projectors(
      ~settings=globals.settings.core,
      ~font_metrics=globals.font_metrics,
      ~secondary_icons=globals.settings.secondary_icons,
      ~inject=x => inject(x),
      ~make_active=signal(MakeActive),
      ~statics,
      Editor.View.mk_projector_model(
        model |> Editor.get_projectors,
        model |> Editor.get_measured,
        model |> Editor.get_selection_ids,
        model |> Editor.get_indicated,
        statics.info_map,
        dynamics,
        selected,
      ),
    );
  let overlays =
    [Node.div(~attrs=[Attr.classes(["code-deco"])], edit_decos)]
    @ [Node.div(~attrs=[Attr.classes(["overlays"])], overlays)]
    @ projectors;
  let code_view =
    view_code_statics(
      ~globals,
      ~overlays,
      ~sort?,
      ~statics,
      ~dynamics,
      model,
    );

  let loc = (e: Pointer.Event.t) =>
    FontMetrics.get_goal(
      ~font_metrics=globals.font_metrics,
      container_target(e.current_target),
      e.loc,
    );

  let move_or_select = (mouse: Pointer.Event.t, pointer_id: int) =>
    switch (mouse) {
    | {shift: Down, _} =>
      Effect.Many([
        signal(MakeActive),
        inject(Select(Resize(Goal(Point(loc(mouse)))))),
      ])
    | {sys: PC, ctrl: Down, _}
    | {sys: Mac, meta: Down, _} =>
      Effect.Many([
        signal(MakeActive),
        inject(Move(Goal(Point(loc(mouse))))),
        inject(Jump(BindingSiteOfIndicatedVar)),
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
          inject(Move(Goal(Point(loc(mouse))))),
        ]);
      | 2 => inject(Select(Smart(2)))
      | 3 => inject(Select(Smart(3)))
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
      inject(Select(Resize(Goal(Point(loc(pointer))))))
    | _ => Effect.Ignore
    };

  Node.div(
    ~attrs=[
      Attr.classes(
        ["cell-item", "code-editor"] @ (selected ? ["selected"] : []),
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
