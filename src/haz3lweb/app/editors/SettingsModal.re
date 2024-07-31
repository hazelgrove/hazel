open Virtual_dom.Vdom;
open Node;
open Haz3lcore;

let view =
    (
      ~inject: Settings.Update.t => Ui_effect.t(unit),
      settings: CoreSettings.Evaluation.t,
    ) => {
  let modal = div(~attrs=[Attr.class_("settings-modal")]);
  let setting = (icon, name, current, action: Settings.Update.t) =>
    div(
      ~attrs=[Attr.class_("settings-toggle")],
      [
        Widgets.toggle(~tooltip=name, icon, current, _ => inject(action)),
        text(name),
      ],
    );
  [
    modal([
      div(
        ~attrs=[Attr.class_("settings-modal-top")],
        [Widgets.button(Icons.x, _ => inject(Evaluation(ShowSettings)))],
      ),
      setting(
        "h",
        "show full step trace",
        settings.stepper_history,
        Evaluation(ShowRecord),
      ),
      setting(
        "|",
        "show case clauses",
        settings.show_case_clauses,
        Evaluation(ShowCaseClauses),
      ),
      setting(
        "λ",
        "show function bodies",
        settings.show_fn_bodies,
        Evaluation(ShowFnBodies),
      ),
      setting(
        "x",
        "show fixpoints",
        settings.show_fixpoints,
        Evaluation(ShowFixpoints),
      ),
      setting(
        Unicode.castArrowSym,
        "show casts",
        settings.show_casts,
        Evaluation(ShowCasts),
      ),
      // Disabled until we have a way to print closures
      // setting(
      //   "🔍",
      //   "show lookup steps",
      //   settings.show_lookup_steps,
      //   Evaluation(ShowLookups),
      // ),
      setting(
        "⏯️",
        "show stepper filters",
        settings.show_stepper_filters,
        Evaluation(ShowFilters),
      ),
      setting(
        "🤫",
        "show hidden steps",
        settings.show_hidden_steps,
        Evaluation(ShowHiddenSteps),
      ),
    ]),
    div(
      ~attrs=[
        Attr.class_("modal-back"),
        Attr.on_mousedown(_ => inject(Evaluation(ShowSettings))),
      ],
      [],
    ),
  ];
};
