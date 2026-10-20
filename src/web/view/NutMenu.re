open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

type setting_item = {
  name: string,
  active: bool,
  setting: Settings.Update.t,
  tooltip: option(string),
};

// COMPONENTS

let item_group = (name: string, ts) => {
  div_c("group", [div_c("name", [text(name)]), div_c("contents", ts)]);
};

let item_class = "top-menu-item";

/* A submenu opens on CSS :hover, which script cannot clear, so an item that
   navigates away would leave its menu hanging open over the new view.
   `dismissed` suppresses the hover rule (nut-menu.css) until the pointer
   leaves the menu. Items that only toggle a setting should not use it: you
   flip several in a row. */
let dismissed_class = "dismissed";

let dismiss = (evt: Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.mouseEvent)): unit =>
  switch (Js_of_ocaml.Js.Opt.to_option(evt##.currentTarget)) {
  | None => ()
  | Some(el) =>
    Util.JsUtil.find_ancestor_with_class(el, item_class)
    |> Option.iter(item =>
         item##.classList##add(Js_of_ocaml.Js.string(dismissed_class))
       )
  };

let submenu = (~tooltip, ~icon, menu) =>
  div(
    ~attrs=[
      clss([item_class]),
      Attr.on_mouseleave(evt => {
        switch (Js_of_ocaml.Js.Opt.to_option(evt##.currentTarget)) {
        | None => ()
        | Some(item) =>
          item##.classList##remove(Js_of_ocaml.Js.string(dismissed_class))
        };
        Effect.Ignore;
      }),
    ],
    [
      div(
        ~attrs=[clss(["submenu-icon"]), Attr.title(tooltip)],
        [div(~attrs=[clss(["icon"])], [icon])],
      ),
      div(~attrs=[clss(["submenu"])], menu),
    ],
  );

// SETTINGS, AS DATA: the settings panel (SettingsPanel) draws these

/* one choice of a multi-way setting, bound to its update */
type choice = {
  c_label: string,
  c_tooltip: string,
  c_active: bool,
  c_set: Effect.t(unit),
};

type row =
  | Toggle(setting_item)
  | Choice(string, string, list(choice)) /* name, description, choices */
  | Action(string, string, Effect.t(unit)); /* label, description, effect */

type section = {
  s_name: string,
  s_rows: list(row),
};

let toggle = (name, active, setting, tooltip) =>
  Toggle({
    name,
    active,
    setting,
    tooltip: Some(tooltip),
  });

let semantics = (~globals: Globals.t): section => {
  s_name: "Semantics",
  s_rows: [
    toggle(
      "Types",
      globals.settings.core.statics,
      Statics,
      "Enable type-directed feedback",
    ),
    toggle(
      "Code completion",
      globals.settings.core.assist,
      Assist,
      "Enable type-directed code completion",
    ),
    toggle(
      "Evaluation",
      globals.settings.core.dynamics,
      Dynamics,
      "Evaluate the program and enable probes",
    ),
  ],
};

let value_display = (~globals: Globals.t): section => {
  let s = globals.settings.core.evaluation;
  {
    s_name: "Value display",
    s_rows: [
      toggle(
        "Functions",
        s.show_fn_bodies,
        Evaluation(ShowFnBodies),
        "Show function bodies in evaluated results",
      ),
      toggle(
        "Cases",
        s.show_case_clauses,
        Evaluation(ShowCaseClauses),
        "Show case clauses in evaluated results",
      ),
      toggle(
        "Fixpoints",
        s.show_fixpoints,
        Evaluation(ShowFixpoints),
        "Show fixpoint expressions in evaluated results",
      ),
      toggle(
        "Tables",
        s.project_tables,
        Evaluation(ProjectTables),
        "Project tables in evaluated results",
      ),
      toggle(
        "Ascriptions",
        s.show_ascriptions,
        Evaluation(ShowAscriptions),
        "Show type ascriptions in evaluated results",
      ),
    ],
  };
};

let stepper = (~globals: Globals.t): section => {
  let s = globals.settings.core.evaluation;
  {
    s_name: "Stepper",
    s_rows: [
      toggle(
        "Show lookups",
        s.show_lookup_steps,
        Evaluation(ShowLookups),
        "Show variable lookup steps in the stepper",
      ),
      toggle(
        "Show hidden",
        s.show_hidden_steps,
        Evaluation(ShowHiddenSteps),
        "Show hidden intermediate steps in the stepper",
      ),
      toggle(
        "Show filters",
        s.show_stepper_filters,
        Evaluation(ShowFilters),
        "Show stepper filter controls",
      ),
      toggle(
        "Show ascription steps",
        s.show_ascription_steps,
        Evaluation(ShowAscriptionSteps),
        "Show type ascription steps in the stepper",
      ),
      toggle(
        "Show case steps",
        s.show_case_steps,
        Evaluation(ShowCaseSteps),
        "Show case expression steps in the stepper",
      ),
      toggle(
        "Proof steps (experimental)",
        s.enable_proof,
        Evaluation(EnableProof),
        "Enable proof-based stepping mode (experimental)",
      ),
    ],
  };
};

let code_display = (~globals: Globals.t): section => {
  module CD = Settings.CompletionDisplay;
  let current = Settings.Model.completion_display(globals.settings);
  let preview = (c_label, c_tooltip, mode) => {
    c_label,
    c_tooltip,
    c_active: current == mode,
    c_set:
      globals.inject_global(Set(Settings.Update.CompletionDisplay(mode))),
  };
  {
    s_name: "Code display",
    s_rows:
      [
        Choice(
          "Completion previews",
          "How completion previews are displayed",
          [
            preview(
              "Quiver",
              "Show completion previews beside their insertion points",
              CD.Quiver,
            ),
            preview(
              "Flag",
              "Raise the caret's completion preview on a flagpole; keep other previews beside their insertion points",
              CD.Flag,
            ),
            preview(
              "None",
              "Hide all completion previews, at the caret and elsewhere",
              CD.Hidden,
            ),
          ],
        ),
        toggle(
          "Whitespace",
          globals.settings.secondary_icons,
          Settings.Update.SecondaryIcons,
          "Show whitespace indicator icons",
        ),
        toggle(
          "Animations",
          globals.settings.core.flip_animations,
          FlipAnimations,
          "Enable flip animations for code changes",
        ),
        toggle(
          "Line numbers",
          globals.settings.line_numbers,
          ToggleLineNumbers,
          "Show line numbers beside the code",
        ),
      ]
      @ (
        globals.settings.line_numbers
          ? [
            toggle(
              "Relative numbers",
              globals.settings.relative_line_numbers,
              ToggleRelativeLineNumbers,
              "Show line numbers relative to cursor position",
            ),
          ]
          : []
      )
      @ [
        toggle(
          "Simple indication",
          globals.settings.simple_indication,
          SimpleIndication,
          "Indicate the caret's term with a minimal arm instead of shard backings",
        ),
      ],
  };
};

let editing = (~globals: Globals.t): section => {
  module FS = Language.CoreSettings.FormatShortcut;
  let current = globals.settings.core.format_shortcut;
  let format = (c_label, c_tooltip, mode) => {
    c_label,
    c_tooltip,
    c_active: current == mode,
    c_set: globals.inject_global(Set(Settings.Update.FormatShortcut(mode))),
  };
  let key = Util.Os.is_mac^ ? "Cmd" : "Ctrl";
  {
    s_name: "Editing",
    s_rows: [
      Choice(
        "Format",
        "What the format shortcut ("
        ++ key
        ++ "+S) does. "
        ++ key
        ++ "+Shift+S always pretty-prints.",
        [
          format("None", "Do not format", FS.Nothing),
          format("Indent", "Re-indent only", FS.Indent),
          format(
            "Spaces",
            "Re-indent and normalize within-line spacing (linebreaks and comments untouched)",
            FS.Spaces,
          ),
          format(
            "Breaks",
            "Full pretty print (may change linebreaks)",
            FS.Breaks,
          ),
        ],
      ),
      toggle(
        "Auto re-indent",
        globals.settings.core.auto_reindent,
        AutoReindent,
        "Re-indent a form's contents when its delimiters complete (experimental)",
      ),
      toggle(
        "Character-level mouse",
        globals.settings.core.selection_chunkiness,
        SelectionChunkiness,
        "When on, mouse drag selects by character. When off (default), mouse drag selects by character inside a token and by whole token beyond; holding Alt (Mac) / Ctrl (PC) while dragging does the reverse. Keyboard Shift+Arrow is always character-level (hold Alt/Ctrl for whole-token).",
      ),
    ],
  };
};

let developer = (~globals: Globals.t): section => {
  s_name: "Developer",
  s_rows:
    [
      toggle(
        "Benchmarks",
        globals.settings.benchmark,
        Settings.Update.Benchmark,
        "Display performance benchmarks",
      ),
      toggle(
        "Elaboration",
        globals.settings.core.elaborate,
        Elaborate,
        "Show elaborated (internal) expressions",
      ),
      toggle(
        "Probe all",
        globals.settings.core.probe_all,
        ProbeAll,
        "Enable probes on all top-level definitions",
      ),
      toggle(
        "Cap undo stack",
        globals.settings.cap_undo_stack,
        CapUndoStack,
        "Cap the undo history stack size",
      ),
      toggle(
        "Ruled lines",
        globals.settings.show_row_lines,
        ShowRowLines,
        "Show horizontal lines between each row of code",
      ),
      toggle(
        "Incremental reuse",
        globals.settings.show_incremental_deco,
        ShowIncrementalDeco,
        "Show incremental evaluator cache hits",
      ),
      toggle(
        "Debug sidebar",
        globals.settings.show_debug_panel,
        ShowDebugPanel,
        "Show the debug info sidebar panel",
      ),
    ]
    @ (
      ExerciseSettings.show_instructor
        ? [
          toggle(
            "Log panel",
            globals.settings.show_log_panel,
            ShowLogPanel,
            "Show the debug log panel",
          ),
        ]
        : []
    ),
};
