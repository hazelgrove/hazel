open Util;
open Language;

/* The keyword label and argument of a proof form (`assume` / `revert` /
 * `generalize` / `have` / `funext`), in the inline-chip wrapper the other
 * step rows use for their arguments.
 *
 * The argument is editable the way the induction scrutinee is: a
 * `SubEditor` window onto the main editor's own segment. The splice's
 * pieces are the proof text's pieces, so there is no write-back step:
 * typing here edits `assume <exp> =>` in the program, which re-checks like
 * any other edit. Each form's argument is child 0 of its tile, so the
 * target is that child in full.
 *
 * Falls back to a read-only render when there is no main editor to
 * splice from (the cell-level stepper) or the segment can't be located. */
let view_arg =
    (
      ~globals: Globals.t,
      ~label: string,
      ~proof: option(Proof.t),
      ~main_editor: option(CodeEditable.Channel.t),
      ~focused: bool,
      ~take_focus: Ui_effect.t(unit),
      arg: option(Any.t),
    )
    : list(WebUtil.Node.t) => {
  let read_only = (e: Any.t) =>
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
      SubEditor.mk(channel.model.editor, ~target)
      |> Option.map(sub =>
           WebUtil.div_c(
             "inline-editor-wrapper",
             [
               CodeEditable.View.view_sub(
                 ~globals,
                 ~channel,
                 ~take_focus,
                 ~focused,
                 sub,
               ),
             ],
           )
         );
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
