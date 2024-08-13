open Util;
open Virtual_dom.Vdom;
open Js_of_ocaml;
open Node;
open Util.Web;
open Widgets;

let export_persistent_data = (~globals: Globals.t) =>
  button_named(
    Icons.sprout,
    _ => {
      globals.export_persistent();
      Ui_effect.Ignore;
    },
    ~tooltip="Export All Persistent Data",
  );

let reset_hazel =
  button(
    Icons.bomb,
    _ => {
      let confirmed =
        JsUtil.confirm(
          "Are you SURE you want to reset Hazel to its initial state? You will lose any existing code that you have written, and course staff have no way to restore it!",
        );
      if (confirmed) {
        JsUtil.clear_localstore();
        Dom_html.window##.location##reload;
      };
      Virtual_dom.Vdom.Effect.Ignore;
    },
    ~tooltip="Clear Local Storage and Reload (LOSE ALL DATA)",
  );

let reparse = (~inject_reparse: option(unit => 'a)) =>
  switch (inject_reparse) {
  | Some(inject_reparse) =>
    button(
      Icons.backpack,
      _ => inject_reparse(),
      ~tooltip="Reparse Current Editor",
    )
  | None =>
    button_d(
      Icons.backpack,
      Effect.Ignore,
      ~tooltip="Reparse Current Editor",
      ~disabled=true,
    )
  };

let settings_menu =
    (
      ~globals as
        {
          inject_global,
          settings:
            {
              core: {evaluation, _} as core,
              benchmark,
              secondary_icons,
              explainThis,
              _,
            },
          _,
        }: Globals.t,
    ) => {
  let toggle = (icon, tooltip, bool, setting) =>
    toggle_named(icon, ~tooltip, bool, _ => inject_global(Set(setting)));
  [
    toggle("τ", "Toggle Statics", core.statics, Statics),
    toggle("⇲", "Toggle Completion", core.assist, Assist),
    toggle("↵", "Show Whitespace", secondary_icons, SecondaryIcons),
    toggle("✓", "Print Benchmarks", benchmark, Benchmark),
    toggle("𝛿", "Toggle Dynamics", core.dynamics, Dynamics),
    toggle("𝑒", "Show Elaboration", core.elaborate, Elaborate),
    toggle(
      "λ",
      "Show Function Bodies",
      evaluation.show_fn_bodies,
      Evaluation(ShowFnBodies),
    ),
    toggle(
      "|",
      "Show Case Clauses",
      evaluation.show_case_clauses,
      Evaluation(ShowCaseClauses),
    ),
    toggle(
      "f",
      "Show fixpoints",
      evaluation.show_fixpoints,
      Evaluation(ShowFixpoints),
    ),
    toggle(
      Unicode.castArrowSym,
      "Show casts",
      evaluation.show_casts,
      Evaluation(ShowCasts),
    ),
    // Disabled until we have a way to print closures
    // toggle(
    //   "🔍",
    //   "Show Lookup Steps",
    //   evaluation.show_lookup_steps,
    //   Evaluation(ShowLookups),
    // ),
    toggle(
      "⏯️",
      "Show Stepper Filters",
      evaluation.show_stepper_filters,
      Evaluation(ShowFilters),
    ),
    toggle(
      "🤫",
      "Show Hidden Steps",
      evaluation.show_hidden_steps,
      Evaluation(ShowHiddenSteps),
    ),
    toggle(
      "?",
      "Show Docs Sidebar",
      explainThis.show,
      ExplainThis(ToggleShow),
    ),
    toggle(
      "👍",
      "Show Docs Feedback",
      explainThis.show_feedback,
      ExplainThis(ToggleShowFeedback),
    ),
  ];
};

let submenu = (~tooltip, ~icon, menu) =>
  div(
    ~attrs=[clss(["top-menu-item"])],
    [
      div(
        ~attrs=[clss(["submenu-icon"]), Attr.title(tooltip)],
        [div(~attrs=[clss(["icon"])], [icon])],
      ),
      div(~attrs=[clss(["submenu"])], menu),
    ],
  );

let view =
    (
      ~globals: Globals.t,
      ~selection: option(Editors.Selection.t),
      ~inject: Editors.Update.t => 'a,
      ~editors: Editors.Model.t,
    ) => [
  a(~attrs=[clss(["nut-icon"])], [Icons.hazelnut]),
  div(
    ~attrs=[clss(["nut-menu"])],
    [
      submenu(~tooltip="Settings", ~icon=Icons.gear, settings_menu(~globals)),
      submenu(
        ~tooltip="Export",
        ~icon=Icons.export,
        Editors.View.export_menu(~globals, editors),
      ),
      submenu(
        ~tooltip="Import",
        ~icon=Icons.import,
        Editors.View.import_menu(~globals, ~inject, editors),
      ),
      reparse(
        ~inject_reparse={
          let update =
            Editors.Selection.handle_key_event(
              ~selection,
              ~event={
                key: D("k"),
                sys: PC,
                shift: Up,
                meta: Down,
                ctrl: Down,
                alt: Up,
              },
              editors,
            );
          Option.map((u, ()) => inject(u), update);
        },
      ),
      reset_hazel,
      link(
        Icons.github,
        "https://github.com/hazelgrove/hazel",
        ~tooltip="Hazel on GitHub",
      ),
      link(Icons.info, "https://hazel.org", ~tooltip="Hazel Homepage"),
    ],
  ),
];
