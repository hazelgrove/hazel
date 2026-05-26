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
      | Equality(_) =>
        let (l, r) =
          switch (active_selection) {
          | _ when model.ctx_entry.is_captured => (None, None) // TODO[Matt]: tooltip explaining why disabled
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
            ~disabled=model.ctx_entry.is_captured || Option.is_none(l),
          ),
          Widgets.button_d(
            // TODO[Matt]: tooltip
            Node.text("==>"),
            r |> Option.value(~default=Ui_effect.Ignore),
            ~disabled=model.ctx_entry.is_captured || Option.is_none(r),
          ),
        ];
      | _ => []
      };
    div_c(
      "assumption-box",
      equality_buttons
      @ [
        Node.text(model.ctx_entry.name ++ ": "),
        CodeViewable.view_any(
          ~globals,
          ~settings=
            Haz3lcore.ExpToSegment.Settings.of_core(
              ~inline=true,
              ~fold_fn_bodies=`Text,
              globals.settings.core,
            ),
          Exp(model.ctx_entry.exp),
        ),
      ],
    );
  };
};
