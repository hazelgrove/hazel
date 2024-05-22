open Js_of_ocaml;
open Haz3lcore;
open Virtual_dom.Vdom;
type editor_id = string;

type model = ReadOnlyEditor.model;

type action = ReadOnlyEditor.action;

let update = ReadOnlyEditor.update;

let calculate = ReadOnlyEditor.calculate;

type event =
  | MakeActive;

module View = {
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
        ~inject: action => Ui_effect.t(unit),
        ~selected: bool,
        ~highlights: option(ColorSteps.colorMap),
        ~overlays: list(Node.t)=[],
        ~sort=?,
        model: model,
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
        switch (highlights) {
        | Some(colorMap) =>
          Deco.color_highlights(ColorSteps.to_list(colorMap))
        | _ => []
        }
      );
    };
    let overlays = edit_decos @ overlays;
    let code_view = ReadOnlyEditor.view(~globals, ~overlays, ~sort?, model);
    let mousedown_overlay =
      selected && globals.mousedown
        ? [mousedown_overlay(~globals, ~inject)] : [];
    let on_mousedown = mousedown_handler(~globals, ~signal, ~inject);
    Node.div(
      ~attr=
        Attr.many([
          Attr.classes(["cell-item"]),
          Attr.classes(["code-editor"]),
          Attr.on_mousedown(on_mousedown),
        ]),
      mousedown_overlay @ [code_view],
    );
  };
};

let view = View.view;
