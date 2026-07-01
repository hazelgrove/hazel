open Js_of_ocaml;
open Haz3lcore;
open Virtual_dom.Vdom;
open Util;

/* Generic embedded editable sub-surface, extracted from CodeEditable so it
 * can be reused independently of projector splices (e.g. by stepper
 * induction editors). Callers supply the region id/content, resolved
 * measurements, caret context, and the actions to run on pointer moves. */

module Model = CodeWithStatics.Model;

  let pointer_target = (current_target: Js.opt(Js.t(Dom_html.element))) =>
    current_target
    |> Js.Opt.get(_, _ => failwith("pointer: no currentTarget"));

  let focus_without_scroll = (elem: Js.t(Dom_html.element)): unit => {
    let _: unit =
      Js.Unsafe.meth_call(
        elem,
        "focus",
        [|
          Js.Unsafe.obj([|("preventScroll", Js.Unsafe.inject(Js._true))|]),
        |],
      );
    ();
  };

  let focus_code_editor = (current_target: Js.opt(Js.t(Dom_html.element))) =>
    switch (
      JsUtil.find_ancestor_with_class(
        pointer_target(current_target),
        "code-editor",
      )
    ) {
    | Some(editor_el) => focus_without_scroll(editor_el)
    | None => ()
    };

  module SubMouseState = Pointer.MkState();

  module SubPointerCapture = {
    let set = (target, pointer_id) =>
      JsUtil.setPointerCapture(pointer_target(target), pointer_id);

    let release = (target, pointer_id) => {
      let target = pointer_target(target);
      if (JsUtil.hasPointerCapture(target, pointer_id)) {
        JsUtil.releasePointerCapture(target, pointer_id);
      };
    };
  };

    let syntax_for_root =
        (~outer: CachedSyntax.t, ~measured: Measured.t, content)
        : CachedSyntax.t => {
      ...outer,
      segment: content,
      /* Draw this viewport with the region's own row frame, supplied by the
       * caller (id-keyed measurements stay tied to the outer cache). */
      measured,
      shape_map: outer.shape_map,
      cached_backpack: Segment.global_missing_shards(content),
    };

    let own_ids = (content: Base.segment): Id.Map.t(unit) =>
      Segment.own_ids(content)
      |> List.fold_left((m, id) => Id.Map.add(id, (), m), Id.Map.empty);

    let surface = (e: Pointer.Event.t) =>
      e.current_target
      |> pointer_target
      |> JsUtil.get_child_with_class(_, "sub-editor-surface")
      |> Option.get;

    let loc = (~globals: Globals.t, e: Pointer.Event.t) => {
      let goal =
        FontMetrics.get_goal(
          ~font_metrics=globals.font_metrics,
          surface(e),
          e.loc,
        );
      Point.{
        ...goal,
        col: max(0, goal.col),
      };
    };

    let view =
        (
          ~globals: Globals.t,
          ~on_activate: Ui_effect.t(unit),
          ~selected: bool,
          ~caret_here: bool,
          ~measured: Measured.t,
          ~move: Point.t => Ui_effect.t(unit),
          ~resize: Point.t => Ui_effect.t(unit),
          ~extra_classes: list(string)=[],
          ~content: Base.segment,
          model: Model.t,
        )
        : Node.t => {
      let zipper = model.editor.state.zipper;
      let outer_syntax = model.editor.syntax;
      let syntax = syntax_for_root(~outer=outer_syntax, ~measured, content);
      let own_ids = own_ids(content);
      let in_root = id => Id.Map.mem(id, own_ids);

      let code_text =
        CodeViewable.view(
          ~globals,
          ~measured=syntax.measured,
          ~term_data=outer_syntax.term_data,
          ~buffer_ids=
            Haz3lcore.Selection.is_buffer(zipper.selection)
              ? outer_syntax.selection_ids : [],
          ~shape_map=syntax.shape_map,
          ~refractor_shape_map=Id.Map.empty,
          content,
        );

      let edit_deco_nodes =
        selected
          ? {
            let caret_nodes =
              caret_here
                ? [
                  CaretDec.view(
                    ~measured=syntax.measured,
                    ~font_metrics=globals.font_metrics,
                    zipper,
                  ),
                ]
                : [];
            let indicated_nodes =
              switch (Indicated.for_decoration(zipper)) {
              | Some({piece: p, _})
                  when in_root(Piece.id(p)) && zipper.selection.content == [] => [
                  Arms.Indicated.term(
                    ~font_metrics=globals.font_metrics,
                    ~syntax,
                    zipper,
                  ),
                ]
              | _ => []
              };
            let selection_nodes =
              caret_here && zipper.selection.content != []
                ? [
                  Highlight.selection(
                    ~measured=syntax.measured,
                    ~shape_map=syntax.shape_map,
                    ~font_metrics=globals.font_metrics,
                    zipper,
                  ),
                ]
                : [];
            let backpack_nodes =
              caret_here
                ? [
                  Backpack.view(
                    ~font_metrics=globals.font_metrics,
                    ~measured=syntax.measured,
                    ~cached_backpack=syntax.cached_backpack,
                    zipper,
                  ),
                ]
                : [];
            caret_nodes @ indicated_nodes @ selection_nodes @ backpack_nodes;
          }
          : [];

      let own_error_ids = List.filter(in_root, model.statics.error_ids);
      let own_warning_ids =
        globals.settings.core.display_warnings
          ? List.filter(in_root, model.statics.warning_ids) : [];
      let error_decos =
        Arms.Errors.of_ids(
          ~font_metrics=globals.font_metrics,
          ~syntax,
          own_error_ids,
        );
      let warning_decos =
        Arms.Errors.of_ids(
          ~is_warning=true,
          ~font_metrics=globals.font_metrics,
          ~syntax,
          own_warning_ids,
        );

      let handle_pointerdown = evt => {
        let e = Pointer.Event.mk(evt);
        let goal = loc(~globals, e);
        switch (e) {
        | {button: Left, shift: Down, _} =>
          focus_code_editor(e.current_target);
          Effect.Many([
            Effect.Stop_propagation,
            Effect.Prevent_default,
            on_activate,
            move(goal),
            resize(goal),
          ]);
        | {button: Left, _} =>
          focus_code_editor(e.current_target);
          SubMouseState.pointerdown(goal);
          SubPointerCapture.set(
            e.current_target,
            Pointer.Event.id_of(evt),
          );
          Effect.Many([
            Effect.Stop_propagation,
            Effect.Prevent_default,
            on_activate,
            move(goal),
          ]);
        | _ => Effect.Ignore
        };
      };
      let handle_pointermove = evt => {
        let e = Pointer.Event.mk(evt);
        let left_button_held = e.buttons land 1 != 0;
        if (!left_button_held && SubMouseState.is_button_down()) {
          SubMouseState.reset();
          Effect.Many([Effect.Stop_propagation, Effect.Prevent_default]);
        } else if (left_button_held && SubMouseState.is_button_down()) {
          let goal = loc(~globals, e);
          if (Point.equals(goal, SubMouseState.get_down_loc())) {
            Effect.Many([Effect.Stop_propagation, Effect.Prevent_default]);
          } else {
            Effect.Many([
              Effect.Stop_propagation,
              Effect.Prevent_default,
              resize(goal),
            ]);
          };
        } else {
          Effect.Ignore;
        };
      };
      let handle_pointerup = evt => {
        let e = Pointer.Event.mk(evt);
        SubMouseState.pointerup(loc(~globals, e));
        SubPointerCapture.release(
          e.current_target,
          Pointer.Event.id_of(evt),
        );
        Effect.Many([Effect.Stop_propagation, Effect.Prevent_default]);
      };

      Node.div(
        ~attrs=[
          Attr.classes(
            ["sub-editor", "inner-editor", "inline-editor-wrapper"]
            @ extra_classes,
          ),
          Attr.on_pointerdown(handle_pointerdown),
          Attr.on_mousemove(handle_pointermove),
          Attr.on_pointerup(handle_pointerup),
        ],
        [
          Node.div(
            ~attrs=[Attr.classes(["sub-editor-surface"])],
            [
              code_text,
              Node.div(
                ~attrs=[Attr.classes(["code-deco"])],
                edit_deco_nodes @ [warning_decos, error_decos],
              ),
            ],
          ),
        ],
      );
    };
