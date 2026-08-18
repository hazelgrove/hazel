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
        /* Syntax-edit channel for the nested stepper: without it, step
         * creation inside a proof scope emits its ProofPatch into the
         * default no-op sink and silently does nothing. */
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
    /* The gap between the last reached expression and the target: a
     * quiet row shaped exactly like a step (same slots, same height) so
     * the chain scans uniformly. The preceding stepper's trailing "?"
     * band lands on the boundary just above it. */
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
