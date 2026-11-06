open Util;
open WebUtil;

module F = (Stepper: StepInterface.STEPPER) => {
  let target_box =
      (
        ~globals: Globals.t,
        ~inject,
        ~take_focus,
        ~hide_stepper,
        ~focus,
        ~is_toplevel,
        ~edit_syntax: Haz3lcore.EditorTransform.patch => Ui_effect.t(unit)=_ =>
                                                                    Ui_effect.Ignore,
        ~main_editor: option(CodeEditable.Channel.t)=None,
        stepper: Stepper.model,
        target: Language.Exp.t,
        reached: Language.Exp.t,
      )
      : list(Node.t) => {
    let stepper_view =
      Stepper.view(
        ~globals,
        ~inject,
        ~take_focus,
        ~hide_stepper,
        ~focus,
        ~is_toplevel,
        ~edit_syntax,
        ~main_editor,
        stepper,
      );
    /* The gap between the last goal reached and the target: a quiet row
     * shaped like a step row, so the chain reads evenly. */
    let step_placeholder = () =>
      Node.div(
        ~attrs=[Attr.classes(["stepper", "cell-result", "step-ellipsis"])],
        [
          div_c(
            "step-border",
            [
              div_c(
                "step-display",
                [
                  Node.div(
                    ~attrs=[Attr.classes(["equiv", "equiv-hidden"])],
                    [Node.text("≡")],
                  ),
                  div_c("step-output", [Node.text("⋯")]),
                ],
              ),
            ],
          ),
        ],
      );
    let target_editor =
      CodeViewable.view_any(
        ~globals,
        ~settings=
          Haz3lcore.ExpToSegment.Settings.of_core(
            ~inline=false,
            globals.settings.core,
          ),
        Exp(target),
      );
    let target_step = () =>
      Node.div(
        ~attrs=[Attr.classes(["stepper", "cell-result", "target-step"])],
        [
          div_c(
            "step-border",
            [
              div_c(
                "step-display",
                [
                  div_c("equiv", [Node.text("≡?")]),
                  div_c("step-output", [target_editor]),
                  Node.text("target"),
                ],
              ),
            ],
          ),
        ],
      );
    if (Language.Exp.fast_equal(target, reached)) {
      stepper_view;
    } else {
      stepper_view @ [step_placeholder(), target_step()];
    };
  };
};
