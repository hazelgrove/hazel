open Util;
open Language;
open WebUtil;

type proof_event =
  | EqualityLeft(string)
  | EqualityRight(string);

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    name: string,
    typ: Typ.t,
    rule: ProofRule.t,
  };
};

module View = {
  let view =
      (
        ~globals: Globals.t,
        ~active_selection:
           option((Exp.t, list(Var.t), proof_event => Ui_effect.t(unit))),
        model: Model.t,
      ) => {
    let equality_buttons =
      switch (model.rule.conclusion) {
      | Equality(_) =>
        let (l, r) =
          switch (active_selection) {
          | Some((exp, _vars, signal)) =>
            let (l, r) = ProofRule.can_eq(model.rule, exp);
            (
              Option.map(_ => signal(EqualityLeft(model.name)), l),
              Option.map(_ => signal(EqualityRight(model.name)), r),
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
      [
        Node.text(model.name),
        Node.text(" : "),
        CodeViewable.view_typ(
          ~globals,
          ~settings=
            Haz3lcore.ExpToSegment.Settings.of_core(
              ~inline=true,
              globals.settings.core,
            ),
          model.typ,
        ),
      ]
      @ equality_buttons,
    );
  };
};
