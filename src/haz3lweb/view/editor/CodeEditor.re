open Js_of_ocaml;
open Haz3lcore;
open Virtual_dom.Vdom;
type editor_id = string;

type model = ReadOnlyEditor.model;

type action = ReadOnlyEditor.action;

let update = ReadOnlyEditor.update;

let calculate = ReadOnlyEditor.calculate;

type event =
  | MakeActive
  | MouseUp
  | MouseDown;

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

  let mousedown_overlay = (~signal, ~inject, ~font_metrics) =>
    Node.div(
      ~attr=
        Attr.many(
          Attr.[
            id("mousedown-overlay"),
            on_mouseup(_ => signal(MouseUp)),
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
              let goal = get_goal(~font_metrics, text_box, e);
              inject(Action.Select(Resize(Goal(Point(goal)))));
            }),
          ],
        ),
      [],
    );

  let mousedown_handler = (~signal, ~inject, ~font_metrics, evt) => {
    let goal =
      get_goal(
        ~font_metrics,
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
        signal(MouseDown),
        signal(MakeActive),
        inject(Action.Move(Goal(Point(goal)))),
      ])
    | (false, 2) => inject(Action.Select(Tile(Current)))
    | (false, 3 | _) => inject(Action.Select(Smart))
    };
  };

  let view =
      (
        ~signal: event => Ui_effect.t(unit),
        ~inject: action => Ui_effect.t(unit),
        ~ui_state: Model.ui_state,
        ~settings: Settings.t,
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
          let ui_state = ui_state;
        });
      Deco.editor(model.editor.state.zipper, model.editor.state.meta.segment)
      @ (
        switch (highlights) {
        | Some(colorMap) =>
          Deco.color_highlights(ColorSteps.to_list(colorMap))
        | _ => []
        }
      );
    };
    let overlays = edit_decos @ overlays;
    let code_view =
      ReadOnlyEditor.view(~ui_state, ~settings, ~overlays, ~sort?, model);
    let mousedown_overlay =
      selected && ui_state.mousedown
        ? [
          mousedown_overlay(
            ~signal,
            ~inject,
            ~font_metrics=ui_state.font_metrics,
          ),
        ]
        : [];
    let on_mousedown =
      mousedown_handler(
        ~signal,
        ~inject,
        ~font_metrics=ui_state.font_metrics,
      );
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
