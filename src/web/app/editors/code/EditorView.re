open Js_of_ocaml;
open Haz3lcorep;
open Virtual_dom.Vdom;
type editor_id = string;
open Util;

module Focus = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('p_f) =
    | Here
    | Projector(Id.t, 'p_f);

  let handle_key_event =
      (
        ~inject: Action.t => Ui_effect.t(unit),
        ~key: Key.t,
        ~enter_prj: (Id.t, Direction.t) => Ui_effect.t(unit),
        ~escape: Direction.t => Ui_effect.t(unit),
        model: Editor.Model.t,
      )
      : Ui_effect.t(unit) => {
    let z = model |> Editor.Model.get_z;
    switch (key, Siblings.neighbors(z.relatives.siblings)) {
    | ({key: D("Tab"), sys: _, shift: Up, meta: Up, ctrl: Up, alt: Up}, _) =>
      /* Attempt to act intelligently when TAB is pressed.
       * TODO: Consider more advanced TAB logic. Instead
       * of simply moving to next hole, if the backpack is non-empty
       * but can't immediately put down, move to next position of
       * interest, which is closet of: nearest position where can
       * put down, farthest position where can put down, next hole */
      let eff =
        Selection.is_buffer(z.selection)
          ? inject(Buffer(Accept))
          : Zipper.can_put_down(z)
              ? inject(Put_down) : inject(Move(Goal(Piece(Grout, Right))));
      Effect.Many([eff, Effect.Stop_propagation, Effect.Prevent_default]);
    | (
        {key: D("ArrowLeft"), sys: _, shift: Up, meta: Up, ctrl: Up, alt: Up},
        (Some(Projector({id, _})), _),
      )
        when z.caret == Outer =>
      Ui_effect.Many([
        enter_prj(id, Right),
        Effect.Stop_propagation,
        Effect.Prevent_default,
      ])
    | (
        {
          key: D("ArrowLeft" | "ArrowUp"),
          sys: _,
          shift: Up,
          meta: Up,
          ctrl: Up,
          alt: Up,
        },
        (None, _),
      )
        when z.caret == Outer && z.relatives.ancestors == [] =>
      Ui_effect.Many([
        escape(Left),
        Effect.Stop_propagation,
        Effect.Prevent_default,
      ])
    | (
        {
          key: D("ArrowRight"),
          sys: _,
          shift: Up,
          meta: Up,
          ctrl: Up,
          alt: Up,
        },
        (_, Some(Projector({id, _}))),
      )
        when z.caret == Outer =>
      Ui_effect.Many([
        enter_prj(id, Left),
        Effect.Stop_propagation,
        Effect.Prevent_default,
      ])
    | (
        {
          key: D("ArrowRight" | "ArrowDown"),
          sys: _,
          shift: Up,
          meta: Up,
          ctrl: Up,
          alt: Up,
        },
        (_, None),
      )
        when z.caret == Outer && z.relatives.ancestors == [] =>
      Ui_effect.Many([
        escape(Right),
        Effect.Stop_propagation,
        Effect.Prevent_default,
      ])
    | _ =>
      switch (Keyboard.handle_key_event(key)) {
      | Some(action) =>
        Ui_effect.Many([
          inject(action),
          Effect.Stop_propagation,
          Effect.Prevent_default,
        ])
      | None => Ui_effect.Ignore
      }
    };
  };

  let applicable_projectors = (m, indicated_kind, mk_projector, read_only) => {
    /* NOTE: This determines which projectors are shown in the projector panel select menu.
     * This approximates but is not fully in sync with the logic in perform
     * which determines what is actually selectable. This should be revisited
     * in the future. This is also a performance problem waiting to happen. */
    let selection = Editor.Model.get_z(m).selection.content;
    let is_projector_indicated =
      switch (indicated_kind, selection) {
      | (Some(_), []) => true
      | (_, [Projector(_)]) => true //TODO
      | _ => false
      };
    let candidate_term =
      if (is_projector_indicated) {
        None;
      } else if (selection == []) {
        /* Note: this will be not exactly the same as the term in the perform logic */
        Editor.Model.indicated_term(
          m,
        );
      } else {
        MakeTerm.for_projection(
          ~of_projector=(~sort as _, ~id as _, _) => Any(), //TODO
          ~log_projector=_ => (),
          selection,
        );
      };
    let is_applicable = kind =>
      OptUtil.Syntax.(
        let* candidate_term = candidate_term;
        /* Note: below elides actually factoring in the editor, because
         * this is awkward to get here in the indicated term case. So
         * this may not work if projector init method actually introspects
         * on the editor */
        let+ _ =
          mk_projector(kind, candidate_term, () =>
            Some(Editor.Model.mk_uncalculated(Zipper.init()))
          );
        kind
      );
    if (read_only) {
      [];
    } else {
      let projectors =
        ProjectorKind.projectors |> List.filter_map(is_applicable);
      switch (indicated_kind) {
      | Some(kind) => ListUtil.lift(kind, projectors)
      | None => projectors
      };
    };
  };

  let indicated_kind = (m, get_kind) =>
    switch (
      m
      |> Editor.Model.get_z
      |> Indicated.piece''
      |> Option.map(((p, _, _)) => p)
    ) {
    | Some(Projector(p)) => Some(get_kind(p.model))
    | _ => None
    };

  let get_cursor_info =
      (
        ~get_cursor_info_pr:
           (
             ~common: Common.t,
             ~inject: 'p_a => Ui_effect.t(unit),
             ~read_only: bool,
             Haz3lcorep.Projector.model,
             'p_f
           ) =>
           Cursor.t,
        ~common: Common.t,
        ~inject: Editor.Update.t => Ui_effect.t(unit),
        ~read_only: bool,
        ~mk_projector:
           (
             ProjectorKind.t,
             Language.Any.t,
             unit => option(Editor.Model.t)
           ) =>
           option(Haz3lcorep.Projector.model),
        ~make_term_prj as _, //TODO(andrew): rm?
        ~get_kind,
        m: Editor.Model.t,
        focus: t('p_f),
      ) => {
    let sys = Os.is_mac^ ? Key.Mac : Key.PC;

    let indicated_kind = indicated_kind(m, get_kind);

    let mk_projection_action = kind =>
      ContextualAction.mk(
        ~section="Projection",
        ProjectorKind.name(kind),
        ~hotkey=?ProjectorKind.shortcut_of(kind),
        inject(Project(SetIndicated(Specific(kind)))),
      );
    let unproject =
      ContextualAction.mk(
        ~section="Projection",
        "Unproject",
        inject(Project(RemoveIndicated)),
      );
    let livelit_action =
      ContextualAction.mk(
        ~hotkey="alt+l",
        ~mdIcon="camera",
        ~section="Projection",
        "Livelit",
        inject(Project(SetIndicated(ChooseLivelit))),
      );
    let projector_actions =
      List.map(
        mk_projection_action,
        applicable_projectors(m, indicated_kind, mk_projector, read_only),
      )
      @ (indicated_kind != None ? [unproject] : [])
      @ (read_only ? [] : [livelit_action]);

    let read_only_actions = [
      ContextualAction.mk(
        ~hotkey="F12",
        ~mdIcon="arrow_forward",
        ~section="Navigation",
        "Go to Definition",
        inject(Jump(BindingSiteOfIndicatedVar)),
      ),
      ContextualAction.mk(
        ~hotkey="shift+tab",
        ~mdIcon="swipe_left_alt",
        ~section="Navigation",
        "Go to Previous Hole",
        inject(Move(Goal(Piece(Grout, Left)))),
      ),
      ContextualAction.mk(
        ~mdIcon="swipe_right_alt",
        ~section="Navigation",
        "Go To Next Hole",
        inject(Move(Goal(Piece(Grout, Right)))),
        // Tab is overloaded so not setting it here
      ),
      ContextualAction.mk(
        ~hotkey=Keyboard.meta(sys) ++ "+d",
        ~mdIcon="select_all",
        ~section="Selection",
        "Select current term",
        inject(Select(Term(Current))),
      ),
      ContextualAction.mk(
        ~mdIcon="select_all",
        ~hotkey=Keyboard.meta(sys) ++ "+a",
        ~section="Selection",
        "Select All",
        inject(Select(All)),
      ),
      ContextualAction.mk(
        ~mdIcon="flip_horizontal",
        ~section="Selection",
        "Toggle Selection Focus",
        inject(Select(ToggleFocus)),
      ),
      ContextualAction.mk(
        ~mdIcon="border_left",
        ~section="Selection",
        ~hotkey=Keyboard.meta(sys) ++ "+alt+shift+left",
        "Set Selection Focus Left",
        inject(Select(SetFocus(Left))),
      ),
      ContextualAction.mk(
        ~mdIcon="border_right",
        ~section="Selection",
        ~hotkey=Keyboard.meta(sys) ++ "+alt+shift+right",
        "Set Selection Focus Right",
        inject(Select(SetFocus(Right))),
      ),
    ];

    let editor_actions = [
      ContextualAction.mk(
        ~hotkey=Keyboard.meta(sys) ++ "+/",
        ~mdIcon="assistant",
        "TyDi Assistant",
        inject(Buffer(Set(TyDi))) // I haven't figured out how to trigger this in the editor
      ),
      ContextualAction.mk(
        // ctrl+k conflicts with the command palette
        ~section="Diagnostics",
        ~mdIcon="refresh",
        "Reparse Current Editor",
        inject(Reparse),
      ),
      ContextualAction.mk(
        ~mdIcon="bolt",
        ~section="Refactoring",
        ~hotkey=Keyboard.meta(sys) ++ "+i",
        "Introduce",
        inject(Introduce),
      ),
    ];

    switch (focus) {
    | Here =>
      Cursor.{
        info:
          Indicated.ci_of(m |> Editor.Model.get_z, common.statics.info_map),
        contextual_actions: projector_actions @ read_only_actions,
        current_projector: Option.map(ProjectorKind.name, indicated_kind),
      }
      |> Cursor.with_actions_if(!read_only, editor_actions)
    | Projector(id, f) =>
      //TODO(andrew): perf problem maybe
      get_cursor_info_pr(
        ~common,
        ~inject=x => inject(Project(Perform(id, x))),
        ~read_only,
        Editor.Model.get_projector_model(id, m),
        f,
      )
    };
  };

  let focus_here = (~focus_parent, m: Editor.Model.t): Ui_effect.t(unit) => {
    Ui_effect.Many([
      Ui_effect.of_sync_fun(
        () => {
          Dom_html.document##getElementById(
            Js.string(Editor.Model.get_web_id(m)),
          )
          |> Js.Opt.to_option
          |> Option.iter(x => x##focus)
        },
        (),
      ),
      focus_parent(Here),
      Effect.Stop_propagation,
    ]);
  };

  let enter =
      (
        ~inject: Editor.Update.t => Ui_effect.t(unit),
        ~focus: t('f) => Ui_effect.t(unit),
        dir: Direction.t,
        m: Editor.Model.t,
      ) =>
    Ui_effect.Many([
      focus_here(~focus_parent=focus, m),
      switch (dir) {
      | Left =>
        inject(
          Move(
            Goal(
              Point({
                row: 0,
                col: 0,
              }),
            ),
          ),
        )
      | Right =>
        inject(
          Move(
            Goal(
              Point({
                row: Int.max_int,
                col: Int.max_int,
              }),
            ),
          ),
        )
      },
    ]);
};

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
      ~common: Common.t,
      ~overlays: list(Node.t)=[],
      ~sort=Sort.root,
      ~background: option(bool)=?,
      editor,
    ) => {
  let code_text_view =
    CodeViewable.view_editor(
      ~secondary_icons=common.secondary_icons,
      ~font_metrics=common.font_metrics,
      ~background?,
      ~sort,
      editor,
    );
  let statics_decos = {
    module Deco =
      Deco.Deco({
        let globals = common;
        let editor = editor;
      });
    Deco.statics();
  };
  WebUtil.div_c(
    "code-container",
    [code_text_view] @ statics_decos @ overlays,
  );
};

let view_code_editable =
    (
      type p_f,
      ~common: Common.t,
      ~view_projector,
      ~projector_to_segment,
      ~mk_status,
      // ~put_clipboard_cache: (string, Segment.t(p_m)) => unit,
      // ~get_clipboard_cache: string => option(Segment.t(p_m)),
      ~inject: Action.t => Ui_effect.t(unit),
      ~focus: Focus.t(p_f) => Ui_effect.t(unit),
      ~focussed: option(Focus.t(p_f)),
      ~escape: Direction.t => Ui_effect.t(unit),
      ~overlays: list(Node.t)=[],
      ~sort,
      ~background=?,
      model: Editor.Model.t,
    ) => {
  let edit_decos = {
    module Deco =
      Deco.Deco({
        let editor = model;
        let globals = common;
      });
    Deco.editor(Editor.Model.get_z(model), focussed == Some(Here));
  };

  let handoff_map: Hashtbl.t(Id.t, (Ui_effect.t(unit), Ui_effect.t(unit))) =
    Hashtbl.create(0);

  let projectors =
    ProjectorView.all(
      ~common,
      ~view_projector,
      ~inject,
      ~make_active=(id, f) => focus(Projector(id, f)),
      ~focus=Focus.focus_here(~focus_parent=focus, model),
      ~focussed=
        switch (focussed) {
        | Some(Here) => None
        | Some(Projector(id, f)) => Some((id, f))
        | None => None
        },
      ~handoff_map,
      ProjectorView.Model.mk(
        ~common,
        ~mk_status,
        Calc.get_saved_exc(model.syntax).projectors,
        Calc.get_saved_exc(model.syntax).measured,
        Calc.get_saved_exc(model.selection_ids),
        switch (Indicated.piece(Editor.Model.get_z(model))) {
        | None => None
        | Some((p, side, _)) => Some((Piece.id(p), side))
        },
        Option.is_some(focussed),
      ),
    );
  let overlays =
    [Node.div(~attrs=[Attr.classes(["code-deco"])], edit_decos)]
    @ [Node.div(~attrs=[Attr.classes(["overlays"])], overlays)]
    @ projectors;
  let code_view =
    view_code_statics(~common, ~overlays, ~sort, ~background?, model);

  let loc = (e: Pointer.Event.t) =>
    FontMetrics.get_goal(
      ~font_metrics=common.font_metrics,
      container_target(e.current_target),
      e.loc,
    );

  let move_or_select = (mouse: Pointer.Event.t, pointer_id: int) =>
    switch (mouse) {
    | {shift: Down, _} =>
      Effect.Many([
        Focus.focus_here(~focus_parent=focus, model),
        inject(Select(Resize(Goal(Point(loc(mouse)))))),
      ])
    | {sys: PC, ctrl: Down, _}
    | {sys: Mac, meta: Down, _} =>
      Effect.Many([
        Focus.focus_here(~focus_parent=focus, model),
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
          Focus.focus_here(~focus_parent=focus, model),
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

  let enter_prj = (id: Id.t, dir: Direction.t) =>
    switch (Hashtbl.find_opt(handoff_map, id)) {
    | None => Ui_effect.Ignore
    | Some((left, right)) =>
      switch (dir) {
      | Left => left
      | Right => right
      }
    };

  Node.div(
    ~attrs=[
      Attr.id(Editor.Model.get_web_id(model)),
      Attr.classes(
        ["cell-item", "code-editor"]
        @ (Option.is_some(focussed) ? ["selected"] : []),
      ),
      Attr.on_pointerdown(evt =>
        move_or_select(Pointer.Event.mk(evt), Pointer.Event.id_of(evt))
      ),
      Attr.on_pointerup(evt =>
        toggle_button(Pointer.Event.mk(evt), Pointer.Event.id_of(evt))
      ),
      Attr.on_mousemove(evt => drag_select(Pointer.Event.mk(evt))),
      Attr.on_wheel(evt => drag_select(Pointer.Event.mk(evt))),
      Key.handler(~f=key =>
        Focus.handle_key_event(~inject, ~key, ~enter_prj, ~escape, model)
      ),
      Attr.on_copy(evt =>
        Ui_effect.Many([
          Effect.of_sync_fun(
            () => {
              let text =
                Printer.of_segment(
                  model |> Editor.Model.get_z |> Zipper.seg_without_buffer,
                  ~projector_to_segment,
                );
              evt##.clipboardData##setData(
                Js.string("text/plain"),
                Js.string(text),
              );
            },
            (),
          ),
          Effect.Stop_propagation,
          Effect.Prevent_default,
        ])
      ),
      Attr.on_cut(evt =>
        Ui_effect.Many([
          Effect.of_sync_fun(
            () => {
              let text =
                Printer.of_segment(
                  model |> Editor.Model.get_z |> Zipper.seg_without_buffer,
                  ~projector_to_segment,
                );
              evt##.clipboardData##setData(
                Js.string("text/plain"),
                Js.string(text),
              );
            },
            (),
          ),
          inject(Destruct(Right)),
          Effect.Stop_propagation,
          Effect.Prevent_default,
        ])
      ),
      Attr.on_paste(_ =>
        Ui_effect.Many([
          (Dom_html.window##.navigator |> Js.Unsafe.coerce)##.clipboard##readText##then_(
            text =>
            inject(Paste(String(text)))
          ),
          Effect.Stop_propagation,
          Effect.Prevent_default,
        ])
      ),
    ],
    [code_view],
  );
} /*   */;

let view =
    (
      ~view_projector,
      ~projector_to_segment,
      ~mk_status,
      ~common: Common.t,
      ~mode: EditorInterface.edit_mode('ed_a, 'ed_f),
      ~overlays: option(list(Node.t))=?,
      ~background: option(bool)=?,
      ~sort,
      model,
    ) =>
  switch (mode) {
  | ReadOnly =>
    view_code_statics(~common, ~overlays?, ~background?, ~sort, model)
  | Editable({inject, escape, take_focus, focus}) =>
    view_code_editable(
      ~common,
      ~view_projector,
      ~projector_to_segment,
      ~mk_status,
      ~inject,
      ~focus=take_focus,
      ~focussed=focus,
      ~escape,
      ~overlays?,
      ~background?,
      ~sort,
      model,
    )
  };

// TODO: Add projectors to read-only view.

// let view =
//     (
//       ~common: Common.t,
//       ~edit_mode: ProjectorInterface.edit_mode('p_k, 'p_m, Focus.t('p_f)),
//       ~overlays: list(Node.t)=[],
//       ~statics: option(CachedStatics.t),
//       ~dynamics: option(Dynamics.Map.t),
//       ~sort,
//       editor,
//     ) => {
//   switch (edit_mode) {
//   | ReadOnly =>
//     view_code_statics(
//       ~globals=common,
//       ~overlays,
//       ~sort,
//       ~statics=statics |> CachedStatics.init,
//       ~dynamics=dynamics |> Dynamics.Map.empty,
//       editor,
//     )
//   };
// };
