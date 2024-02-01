open Virtual_dom.Vdom;
open Node;
open Haz3lcore;

let stepper_view =
    (
      ~inject,
      ~settings: CoreSettings.Evaluation.t,
      ~font_metrics,
      ~result_key,
      ~read_only: bool,
      stepper: Stepper.t,
    ) => {
  let step_dh_code =
      (
        ~next_steps,
        {previous_step, hidden_steps, chosen_step, d}: Stepper.step_info,
      ) =>
    div(
      ~attr=Attr.classes(["result"]),
      [
        DHCode.view(
          ~inject,
          ~settings,
          ~selected_hole_instance=None,
          ~font_metrics,
          ~width=80,
          ~previous_step,
          ~chosen_step,
          ~hidden_steps,
          ~result_key,
          ~next_steps,
          d,
        ),
      ],
    );
  let history = Stepper.get_history(~settings, stepper);
  switch (history) {
  | [] => []
  | [hd, ...tl] =>
    let button_back =
      Widgets.button_d(
        Icons.undo,
        inject(UpdateAction.StepperAction(result_key, StepBackward)),
        ~disabled=!Stepper.can_undo(~settings, stepper),
        ~tooltip="Step Backwards",
      );
    let hide_stepper =
      Widgets.toggle(~tooltip="Show Stepper", "s", true, _ =>
        inject(UpdateAction.ToggleStepper(result_key))
      );
    let show_history =
      Widgets.toggle(~tooltip="Show History", "h", settings.stepper_history, _ =>
        inject(Set(Evaluation(ShowRecord)))
      );
    let eval_settings =
      Widgets.button(Icons.gear, _ =>
        inject(Set(Evaluation(ShowSettings)))
      );
    let current =
      div(
        ~attr=Attr.classes(["cell-result"]),
        read_only
          ? [
            div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
            step_dh_code(~next_steps=[], hd),
          ]
          : [
            div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
            step_dh_code(~next_steps=Stepper.get_next_steps(stepper), hd),
            button_back,
            eval_settings,
            show_history,
            hide_stepper,
          ],
      );
    let dh_code_previous = step_dh_code;
    let previous_step = (step: Stepper.step_info) => {
      div(
        ~attr=Attr.classes(["cell-result"]),
        [
          div(~attr=Attr.class_("equiv"), [Node.text("≡")]),
          dh_code_previous(~next_steps=[], step),
          div(
            ~attr=Attr.classes(["stepper-justification"]),
            step.chosen_step
            |> Option.map((chosen_step: EvaluatorStep.step) =>
                 chosen_step.knd |> Stepper.get_justification |> Node.text
               )
            |> Option.to_list,
          ),
        ],
      );
    };
    List.map(previous_step, tl) |> List.rev_append(_, [current]);
  };
};
