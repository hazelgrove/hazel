open Util;
open Language;
open WebUtil;

type proof_event =
  | EqualityLeft(string)
  | EqualityRight(string);

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {ctx_entry: ProofCtx.entry};
};

module View = {
  let view =
      (
        ~globals: Globals.t,
        ~info_map,
        ~env: Environment.t(Exp.t),
        ~active_selection:
           option((Exp.t, list(Var.t), proof_event => Ui_effect.t(unit))),
        model: Model.t,
      ) => {
    let equality_buttons =
      switch (model.ctx_entry.rule.conclusion) {
      /* A bare boolean conclusion `P` is read as `P == true`. Only the
       * forward rewrite is offered: `true` to `P` would match every
       * `true` in the goal. Written as an `axiomrev` step it still works. */
      | Equality(_)
      | BoolFact(_) =>
        let (l, r) =
          switch (active_selection) {
          /* A shadowed fact is still in scope and still true, but its name
           * reaches a nearer fact, so a step citing it would rewrite with
           * that one instead. */
          | _ when model.ctx_entry.is_captured || model.ctx_entry.is_shadowed => (
              None,
              None,
            ) // TODO[Matt]: tooltip explaining why disabled
          | Some((exp, _vars, signal)) =>
            let exp = exp |> DHExp.strip_ascriptions;
            let (l, r) =
              ProofRule.Rewrite.apply(
                ~info_map,
                ~env,
                model.ctx_entry.rule,
                exp,
              );
            (
              Option.map(_ => signal(EqualityLeft(model.ctx_entry.name)), l),
              Option.map(
                _ => signal(EqualityRight(model.ctx_entry.name)),
                r,
              ),
            );
          | None => (None, None)
          };
        [
          Widgets.button_d(
            // TODO[Matt]: tooltip
            Node.text("<=="),
            l |> Option.value(~default=Ui_effect.Ignore),
            ~disabled=Option.is_none(l),
          ),
          Widgets.button_d(
            // TODO[Matt]: tooltip
            Node.text("==>"),
            r |> Option.value(~default=Ui_effect.Ignore),
            ~disabled=Option.is_none(r),
          ),
        ];
      | _ => []
      };
    let code_settings =
      ProofMarkView.code_view_settings(globals.settings.core);
    /* A fact with a bare boolean conclusion is used as `P == true`, and
     * says so. */
    let reading_note =
      switch (ProofRule.BoolReading.equation(model.ctx_entry.rule)) {
      | None => []
      | Some(reading) => [
          div_c(
            "assumption-reading",
            [
              Node.text("reads as: "),
              CodeViewable.view_any(
                ~globals,
                ~settings=code_settings,
                Exp(reading),
              ),
            ],
          ),
        ]
      };

    let shadowed_note =
      model.ctx_entry.is_shadowed
        ? [
          div_c(
            "assumption-shadowed",
            [
              Node.text(
                "shadowed — `"
                ++ model.ctx_entry.name
                ++ "` names a nearer fact; cite this one with `alias`",
              ),
            ],
          ),
        ]
        : [];
    Node.div(
      ~attrs=[
        Attr.classes(
          ["assumption-box"]
          @ (model.ctx_entry.is_shadowed ? ["shadowed"] : []),
        ),
      ],
      equality_buttons
      @ [
        Node.text(model.ctx_entry.name ++ ": "),
        CodeViewable.view_any(
          ~globals,
          ~settings=code_settings,
          Exp(model.ctx_entry.exp),
        ),
      ]
      @ reading_note
      @ shadowed_note,
    );
  };
};
