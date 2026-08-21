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
      /* `BoolFact` is a bare-boolean conclusion read as `P == true`
       * (`ProofRule.with_bool_fact_reading`, granted by `AxiomsBox`).
       * It gets the same buttons: `can_eq` supplies the forward
       * rewrite (`P` |-> `true`) and `None` for the reverse, so `<==`
       * simply comes out disabled — rule DISCOVERY does not offer
       * `true` |-> `P`, which would match every `true` in the goal. The
       * reverse direction is still available by writing an explicit
       * `axiomrev` step. */
      | Equality(_)
      | BoolFact(_) =>
        let (l, r) =
          switch (active_selection) {
          /* A SHADOWED entry is not citable: a nearer fact of the same
           * name is what that name reaches (docs/prover-obligations.md,
           * "Hypothesis naming"). The fact is still shown — it is in
           * scope and still true — but offering a rewrite button would
           * emit an `axiom <name>` step that rewrites with the OTHER
           * fact, so the buttons come out disabled and the row is
           * de-emphasised. */
          | _ when model.ctx_entry.is_captured || model.ctx_entry.is_shadowed => (
              None,
              None,
            ) // TODO[Matt]: tooltip explaining why disabled
          | Some((exp, _vars, signal)) =>
            let exp = exp |> DHExp.strip_ascriptions;
            let (l, r) =
              ProofRule.can_eq(~info_map, ~env, model.ctx_entry.rule, exp);
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
            ~disabled=
              model.ctx_entry.is_captured
              || model.ctx_entry.is_shadowed
              || Option.is_none(l),
          ),
          Widgets.button_d(
            // TODO[Matt]: tooltip
            Node.text("==>"),
            r |> Option.value(~default=Ui_effect.Ignore),
            ~disabled=
              model.ctx_entry.is_captured
              || model.ctx_entry.is_shadowed
              || Option.is_none(r),
          ),
        ];
      | _ => []
      };
    let code_settings =
      Haz3lcore.ExpToSegment.Settings.of_core(
        ~inline=true,
        ~fold_fn_bodies=`Text,
        globals.settings.core,
      );
    /* Display honesty: a rule stated with a bare-boolean conclusion is
     * USED as the equation `P == true`. Say so, in the same code
     * rendering as the statement itself, rather than leaving the
     * interpretation implicit (docs/prover-obligations.md §2.1). */
    let reading_note =
      switch (ProofRule.bool_reading_exp(model.ctx_entry.rule)) {
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
    /* Say WHY a shadowed row is inert, rather than just greying it: the
     * name is taken by a nearer introduction of the same fixed name. */
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
