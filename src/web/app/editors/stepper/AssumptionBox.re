open Util;
open Language;
open WebUtil;

type proof_event =
  | EqualityLeft(Exp.t)
  | EqualityRight(Exp.t);

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {ctx_entry: ProofCtx.entry};
};

module View = {
  let view =
      (
        ~globals: Globals.t,
        ~env: ClosureEnvironment.t,
        ~active_selection:
           option((Exp.t, list(Var.t), proof_event => Ui_effect.t(unit))),
        model: Model.t,
      ) => {
    let equality_buttons =
      switch (model.ctx_entry.rule.conclusion) {
      | Equality(_) =>
        let (l, r) =
          switch (active_selection) {
          | Some((exp, _vars, signal)) =>
            let (l, r) = ProofRule.can_eq(~env, model.ctx_entry.rule, exp);
            (
              Option.map(e => signal(EqualityLeft(e)), l),
              Option.map(e => signal(EqualityRight(e)), r),
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
    div_c(
      "assumption-box",
      equality_buttons
      @ [
        Node.text(model.ctx_entry.name ++ ": "),
        CodeViewable.view_typ(
          ~globals,
          ~settings=
            Haz3lcore.ExpToSegment.Settings.of_core(
              ~inline=true,
              ~fold_fn_bodies=`Text,
              globals.settings.core,
            ),
          model.ctx_entry.typ,
        ),
      ],
    );
  };
};
