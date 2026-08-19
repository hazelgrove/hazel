open Util;
open Language;

/* Shared rendering for the wrapping proof forms (`assume` / `revert` /
 * `generalize`): the keyword label plus the form's expression argument,
 * in the same inline-chip wrapper the other step rows use for their
 * arguments (cf. InductionStep's "Induction on:" scrutinee).
 *
 * The argument is EDITABLE, by exactly the mechanism the induction
 * scrutinee uses: a `SubEditor` window onto the main editor's own
 * segment (see SubEditor.re). The splice's pieces ARE the proof text's
 * pieces, so there is no write-back step and nothing to keep in sync —
 * typing here edits `assume <exp> =>` in the program, which re-checks
 * like any other edit. Each of the three forms is
 * `mk_pre_c(L, [kw, "=>"], ..., [Exp])`, i.e. one child slot holding
 * the whole expression, so the target is that child in full.
 *
 * Falls back to a read-only render (no editor chrome) when there is no
 * main editor to splice from — the cell-level stepper, or a moment when
 * the backing segment can't be located. */
let view_arg =
    (
      ~globals: Globals.t,
      ~label: string,
      ~proof: option(Proof.t),
      ~main_editor: option(CodeEditable.Channel.t),
      ~focused: bool,
      ~take_focus: unit => Ui_effect.t(unit),
      arg: option(Exp.t),
    )
    : list(WebUtil.Node.t) => {
  let read_only = (e: Exp.t) =>
    WebUtil.div_c(
      "proof-form-arg-static",
      [
        CodeViewable.view_any(
          ~globals,
          ~settings=
            Haz3lcore.ExpToSegment.Settings.of_core(
              ~inline=true,
              ~fold_fn_bodies=`Text,
              globals.settings.core,
            ),
          Exp(e),
        ),
      ],
    );
  let editable = () =>
    switch (main_editor, proof) {
    | (Some(channel), Some(p)) =>
      let target = SubEditor.Target.child(~anchor=Proof.rep_id(p), 0);
      switch (SubEditor.mk(channel.model.editor, ~target)) {
      | Some(sub) =>
        Some(
          WebUtil.div_c(
            "inline-editor-wrapper",
            [
              CodeEditable.View.view(
                ~globals,
                ~signal=
                  fun
                  | MakeActive => take_focus(),
                ~edit_mode=
                  EditMode.Editable({
                    /* Perform actions are rewritten to PerformConfined
                     * inside CodeEditable.View.view when a sub-editor is
                     * given, so edits stay inside the slot. */
                    inject: channel.inject,
                    escape: _ => Ui_effect.Ignore,
                    take_focus: _ => Ui_effect.Ignore,
                    focus: focused ? Some() : None,
                  }),
                ~dynamics=Dynamics.Map.empty,
                ~sub_editor=Some(sub),
                channel.model,
              ),
            ],
          ),
        )
      | None => None
      };
    | _ => None
    };
  switch (arg) {
  | None => []
  | Some(e) => [
      WebUtil.div_c(
        "proof-form-arg",
        [WebUtil.Node.text(label)]
        @ [editable() |> Option.value(~default=read_only(e))],
      ),
    ]
  };
};
