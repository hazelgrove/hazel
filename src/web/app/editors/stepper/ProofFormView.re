open Util;
open Language;

/* Shared rendering for the wrapping proof forms (`assume` / `revert` /
 * `generalize` / `have` / `funext`): the keyword label plus the form's
 * argument,
 * in the same inline-chip wrapper the other step rows use for their
 * arguments (cf. InductionStep's "Induction on:" scrutinee).
 *
 * The argument is EDITABLE, by exactly the mechanism the induction
 * scrutinee uses: a `SubEditor` window onto the main editor's own
 * segment (see SubEditor.re). The splice's pieces ARE the proof text's
 * pieces, so there is no write-back step and nothing to keep in sync —
 * typing here edits `assume <exp> =>` in the program, which re-checks
 * like any other edit. Each of the three forms is
 * `mk_pre_c(L, [kw, ...], ..., [child])`, i.e. one child slot holding
 * the whole argument, so the target is that child in full. The slot's
 * SORT differs — Exp for the four expression forms, Pat for `funext`'s
 * binder — but nothing about the splice does, hence `view_any_arg` with
 * a thin wrapper per sort.
 *
 * Falls back to a read-only render (no editor chrome) when there is no
 * main editor to splice from — the cell-level stepper, or a moment when
 * the backing segment can't be located. */
let view_any_arg =
    (
      ~globals: Globals.t,
      ~label: string,
      ~proof: option(Proof.t),
      ~main_editor: option(CodeEditable.Channel.t),
      ~focused: bool,
      ~take_focus: unit => Ui_effect.t(unit),
      arg: option(Language.Any.t),
    )
    : list(WebUtil.Node.t) => {
  let read_only = (e: Language.Any.t) =>
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
          e,
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

/* The Exp-argument forms (`assume` / `revert` / `generalize` / `have`). */
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
    : list(WebUtil.Node.t) =>
  view_any_arg(
    ~globals,
    ~label,
    ~proof,
    ~main_editor,
    ~focused,
    ~take_focus,
    arg |> Option.map((e): Language.Any.t => Exp(e)),
  );

/* `funext`'s argument is a PAT, not an Exp — `["funext", "=>"]` with a
 * Pat child. The splice is identical (child 0 of the form in full), and
 * so is the read-only fallback; only the sort of the thing being
 * rendered differs. */
let view_pat_arg =
    (
      ~globals: Globals.t,
      ~label: string,
      ~proof: option(Proof.t),
      ~main_editor: option(CodeEditable.Channel.t),
      ~focused: bool,
      ~take_focus: unit => Ui_effect.t(unit),
      arg: option(Pat.t),
    )
    : list(WebUtil.Node.t) =>
  view_any_arg(
    ~globals,
    ~label,
    ~proof,
    ~main_editor,
    ~focused,
    ~take_focus,
    arg |> Option.map((p): Language.Any.t => Pat(p)),
  );
